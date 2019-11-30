/*
 *    Copyright (c) 2019 Pythian and Valentin Nikotin
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package plutarch.server.data.persistence

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{ Files, Path, Paths, StandardOpenOption }
import java.util.concurrent.ConcurrentHashMap

import plutarch.server.data.objects.MemoryObjectStore
import plutarch.server.data.persistence.util.{ FileClassStore, IndexedAppendableFileStore, VersionedFileClassStore }
import plutarch.shared.data.DataObject

import scala.concurrent.{ Await, ExecutionContext, Future, Promise }
import boopickle.Default.{ Pickle, Unpickle }
import boopickle.Default._
import com.typesafe.scalalogging.LazyLogging
import FileObjectsStore.{ DataObjectState, Version }
import akka.actor.ActorSystem
import plutarch.shared.collection.ImmutableIntervalSet

import scala.concurrent.duration._

object FileObjectsStore extends LazyLogging {

  case class Version(index: Int, version: Long) {
    def inc: Version = Version(index, version + 1)
  }
  case class State(names: ConcurrentHashMap[String, Version] = new ConcurrentHashMap[String, Version]())

  case class DataObjectState(id: Int, color: String, first: Long, last: Long, version: Long) extends VersionedFileClassStore.Versioned

  val ssize = 41

  val serDe: FileClassStore.SerDe[DataObjectState] = new FileClassStore.SerDe[DataObjectState] {
    def ser(t: DataObjectState): ByteBuffer = {
      val buf = Pickle.intoBytes(t)
      assert(buf.limit <= ssize)
      buf
    }
    def de(buffer: ByteBuffer): DataObjectState = Unpickle[DataObjectState].fromBytes(buffer)
  }

  val partialNamesPath: Path = Paths.get("names.dat")
  val partialStatesPath: Path = Paths.get("states.dat")

  def create(path: Path, latency: Int)(implicit system: ActorSystem): FileObjectsStore = {
    import system.dispatcher
    logger.info(s"Creating FileObjectsStore($path)")
    Files.createDirectories(path)
    val namesPath = path.resolve(partialNamesPath)
    val statesPath = path.resolve(partialStatesPath)
    val names = IndexedAppendableFileStore.create(namesPath)
    val states = VersionedFileClassStore.create[DataObjectState](statesPath, ssize, serDe)
    val res = new FileObjectsStore(names, states, State(), 0, MemoryObjectStore.emptyState, latency)
    res.check(Long.MinValue, "total", Some("#43CD80"))
    res.check(Long.MaxValue, "total")
    res
  }

  def readNames(path: Path): Iterator[String] = {
    val ch = FileChannel.open(path, StandardOpenOption.READ)
    assert(ch.size() <= Int.MaxValue)
    val buffer = ByteBuffer.allocate(ch.size().toInt)
    ch.read(buffer)
    ch.close()
    buffer.flip()
    new Iterator[String] {
      def hasNext: Boolean = buffer.hasRemaining
      def next(): String = {
        Unpickle[String].fromBytes(buffer)
      }
    }
  }

  def readStates(path: Path): Iterator[DataObjectState] =
    VersionedFileClassStore.read[DataObjectState](path, ssize, serDe)

  def open(path: Path, latency: Int)(implicit system: ActorSystem): FileObjectsStore = {
    import system.dispatcher
    logger.info(s"Opening FileObjectsStore($path)")
    val namesPath = path.resolve(partialNamesPath)
    val statesPath = path.resolve(partialStatesPath)

    val state = State()
    var initId = 0

    val nameToId = Map.newBuilder[String, Int]
    val idToObjState = Map.newBuilder[Int, MemoryObjectStore.DataObjectState]
    var times = ImmutableIntervalSet.empty()

    var idx = 0
    for ((name, doState) ← readNames(namesPath) zip readStates(statesPath)) {
      state.names.put(name, Version(idx, doState.version))
      nameToId += (name -> doState.id)
      idToObjState += (doState.id ->
        MemoryObjectStore.DataObjectState(
          DataObject(doState.id, name, doState.color), doState.first, doState.last))
      times = times.add(doState.first, doState.last, doState.id)
      initId = initId max (doState.id + 1)
      idx += 1
    }
    val initIndex = idx

    val states = VersionedFileClassStore.open[DataObjectState](statesPath, ssize, serDe)
    val names = IndexedAppendableFileStore.open(namesPath, initIndex)
    val memState = MemoryObjectStore.State(nameToId.result(), idToObjState.result(), times)

    new FileObjectsStore(names, states, state, initId, memState, latency)
  }

}

class FileObjectsStore(
    val names:  IndexedAppendableFileStore,
    val states: VersionedFileClassStore[DataObjectState],
    state:      FileObjectsStore.State,
    initId:     Int,
    initState:  MemoryObjectStore.State,
    latency:    Int)(implicit executionContext: ExecutionContext) extends MemoryObjectStore(initId, initState, latency) {

  /*
  * Ops for persistent store:
  * 1. append only: save names in order, keep id = order - AppendableFileStore
  * 2. updatable first/last - fixed size FileClassStore
  * */

  def check(t: Long, name: String, initColor: Option[String]): DataObject = {
    val res = checkState(t, name, initColor)
    res match {
      case MemoryObjectStore.Changed(objState) ⇒
        val obj = objState.obj
        while (!state.names.containsKey(name)) {
          Thread.sleep(10)
        }
        val version = state.names.get(name).inc
        state.names.put(name, version)
        val newState = DataObjectState(obj.id, obj.color, objState.first, objState.last, version.version)
        // race condition with overwritten state!!!
        // temporary solution
        Await.result(states.set(version.index, newState), 1 seconds)
      case MemoryObjectStore.New(objState) ⇒
        val obj = objState.obj
        val nameBuffer = Pickle.intoBytes(name)
        val fres = names.put(nameBuffer).flatMap { saved ⇒
          //Thread.sleep(50) // to test race conditions type2
          state.names.put(name, Version(saved.idx, 0))
          val newState = DataObjectState(obj.id, obj.color, objState.first, objState.last, 0)
          states.set(saved.idx, newState)
        }
        // temporary solution
        Await.result(fres, 1 seconds)
      case _ ⇒
    }
    res.state.obj
  }

  def commit(): Unit = {
    names.commit()
    states.commit()
  }

  override def close(): Unit = {
    super.close()
    names.close()
    states.close()
  }

}
