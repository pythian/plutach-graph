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
import java.util.concurrent.atomic.AtomicLong

import boopickle.Default.Pickle
import boopickle.Default._
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.persistence.util.AppendableFileStore
import plutarch.server.data.raw.Raw
import plutarch.server.data.report.RawStoreFileReport

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

object FileRawStore {

  type TData = (Long, Seq[(String, Double)])
  type TIter = Iterator[TData]

  def create(path: Path)(implicit executionContext: ExecutionContext): FileRawStore = {
    Files.createDirectory(path)
    val dataPath = path.resolve(Paths.get("data.dat"))
    val afs = AppendableFileStore.create(dataPath)
    new FileRawStore(afs, Long.MaxValue, Long.MinValue)
  }

  def open(path: Path)(implicit executionContext: ExecutionContext): FileRawStore = {
    val dataPath = path.resolve(Paths.get("data.dat"))
    val afs = AppendableFileStore.open(dataPath)
    val iter = getIterator(dataPath)
    val (fst, cur) = if (iter.isEmpty) {
      (Long.MaxValue, Long.MinValue)
    } else {
      val fst = iter.next()._1
      val iterable = new Iterable[TData] {
        def iterator: TIter = iter
      }
      (fst, iterable.last._1)
    }
    new FileRawStore(afs, fst, cur)
  }

  private def getIterator(path: Path): TIter = {
    val ch = FileChannel.open(path, StandardOpenOption.READ)
    assert(ch.size() <= Int.MaxValue)
    val buffer = ByteBuffer.allocate(ch.size().toInt)
    ch.read(buffer)
    ch.close()
    buffer.flip()
    new TIter {
      def hasNext: Boolean = buffer.hasRemaining
      def next(): TData = Unpickle[TData].fromBytes(buffer)
    }
  }

}

class FileRawStore(
    afs:         AppendableFileStore,
    initFirst:   Long,
    initCurrent: Long)(implicit executionContext: ExecutionContext) extends Raw with LazyLogging {

  import FileRawStore._

  private var frozen = false

  private val fst = new AtomicLong(initFirst)
  private val cur = new AtomicLong(initCurrent)

  def put(t: Long, values: Seq[(String, Double)]): Future[Unit] = {
    if (frozen) {
      throw new RuntimeException(s"FileRawStore store is frozen")
    }
    if (first == Long.MaxValue) {
      fst.compareAndSet(Long.MaxValue, t)
    }
    val buffer = Pickle.intoBytes((t, values))
    val p = Promise[Unit]()
    afs.put(buffer).onComplete {
      case Success(_) ⇒
        var c = cur.get
        while (t > c) {
          cur.compareAndSet(c, t)
          c = cur.get
        }
        if (t < c) {
          logger.warn(s"Possible unordered saving in FileRawStore(${afs.path}), t=$t, current=$c")
        }
        p.complete(Success())
      case Failure(ex) ⇒
        logger.error(s"Error during saving into FileRawStore(${afs.path})", ex)
        p.failure(ex)
    }
    p.future
  }
  def commit(): Unit = {
    afs.commit()
  }
  def first: Long = fst.get()
  def current: Long = cur.get()
  def iterator: TIter = getIterator(afs.path)
  override def fzeeze(): Unit = {
    frozen = true
    cur.set(Long.MaxValue / 2)
  }
  override def close(): Unit = {
    afs.close()
  }

  override def report: RawStoreFileReport = {
    RawStoreFileReport()
  }
}
