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
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ Future, Promise }

object AppendableFileStore extends LazyLogging {
  case class AioContext[T](buffer: ByteBuffer, pos: Long, len: Int, promise: Promise[T])

  def create(path: Path): AppendableFileStore = {
    logger.info(s"Creating AppendableFileStore($path)")
    val afs = AsyncFileStore.create(path)
    new AppendableFileStore(afs, path)
  }

  def open(path: Path): AppendableFileStore = {
    logger.info(s"Opening AppendableFileStore($path)")
    val afs = AsyncFileStore.open(path)
    new AppendableFileStore(afs, path)
  }
}

class AppendableFileStore(afs: AsyncFileStore, val path: Path) {
  private val currentPos = new AtomicLong(afs.aio.size())

  def put(buffer: ByteBuffer): Future[Long] = {
    val len = buffer.remaining()
    val pos = currentPos.getAndAdd(len)
    afs.put(pos, buffer)
  }

  def get(pos: Long, len: Int): Future[ByteBuffer] = afs.get(pos, len)

  def current: Long = currentPos.get()
  def commit(): Unit = afs.commit()
  def close(): Unit = afs.close()
}
