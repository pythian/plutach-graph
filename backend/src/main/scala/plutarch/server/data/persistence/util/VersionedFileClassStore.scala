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

package plutarch.server.data.persistence.util

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{ Path, StandardOpenOption }

import akka.pattern.ask
import akka.actor.{ Actor, ActorSystem, Props }
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.persistence.util.FileClassStore.SerDe
import plutarch.server.data.persistence.util.VersionedFileClassStore.Versioned

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Success, Try }

object VersionedFileClassStore extends LazyLogging {

  trait Versioned {
    def version: Long
  }

  def read[T](path: Path, size: Int, serDe: SerDe[T]): Iterator[T] = {
    val ch = FileChannel.open(path, StandardOpenOption.READ)
    val buffer = ByteBuffer.allocate(size)
    def read(): Option[T] = if (ch.isOpen) {
      buffer.clear()
      ch.read(buffer)
      buffer.flip()
      if (buffer.hasRemaining) {
        Some(serDe.de(buffer))
      } else {
        ch.close()
        None
      }
    } else {
      None
    }
    new Iterator[T] {
      var value: Option[T] = read()
      def hasNext: Boolean = value.nonEmpty
      def next(): T = {
        val ret = value.get
        value = read()
        ret
      }
    }
  }

  def create[T <: Versioned](path: Path, size: Int, serDe: SerDe[T])(implicit actorSystem: ActorSystem): VersionedFileClassStore[T] = {
    logger.info(s"Creating VersionedFileClassStore($path)")
    val afs = AsyncFileStore.create(path)
    new VersionedFileClassStore[T](afs, size, serDe)
  }

  def open[T <: Versioned](path: Path, size: Int, serDe: SerDe[T])(implicit actorSystem: ActorSystem): VersionedFileClassStore[T] = {
    logger.info(s"Opening VersionedFileClassStore($path)")
    val afs = AsyncFileStore.open(path)
    new VersionedFileClassStore[T](afs, size, serDe)
  }

  private class VersionedKeeper[T <: Versioned](afs: AsyncFileStore, size: Int, serDe: FileClassStore.SerDe[T]) extends Actor {
    import context.dispatcher
    private val versions = mutable.Map.empty[Int, Long]
    private def init(): Unit = {
      var idx = 0
      for (t ← read(afs.path, size, serDe)) {
        val pos = idx * size
        versions += (pos -> t.version)
        idx += 1
      }
    }
    init()
    def receive: Actor.Receive = {
      case Put(pos, data, version) ⇒
        versions.get(pos) match {
          case Some(currentVersion) if currentVersion > version ⇒
            sender() ! Success(-1L)
          case _ ⇒
            val recepient = sender()
            versions += (pos -> version)
            afs.put(pos, data).onComplete(recepient ! _)
        }
      case Close ⇒
        afs.close()
        context.stop(self)
    }
  }

  private sealed trait Protocol
  private case class Put(pos: Int, data: ByteBuffer, version: Long) extends Protocol
  private case object Close extends Protocol
}

class VersionedFileClassStore[T <: Versioned](afs: AsyncFileStore, size: Int, serDe: FileClassStore.SerDe[T])(implicit actorSystem: ActorSystem) {
  import VersionedFileClassStore._
  import actorSystem.dispatcher
  implicit val timeout = Timeout(10000 millis)

  private val actor = actorSystem.actorOf(Props(new VersionedKeeper(afs, size, serDe)))

  def set(index: Int, value: T): Future[Long] = {
    val pos = index * size
    val data = serDe.ser(value)
    (actor ? Put(pos, data, value.version)).mapTo[Try[Long]].map(_.get)
  }

  def get(index: Int): Future[T] = {
    val pos = index * size
    afs.get2(pos, size).map(serDe.de)
  }

  def commit(): Unit = afs.commit()
  def close(): Unit = actor ! Close

}
