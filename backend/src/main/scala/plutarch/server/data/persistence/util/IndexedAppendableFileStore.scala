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
import java.util.concurrent.atomic.AtomicReference
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.Future

object IndexedAppendableFileStore extends LazyLogging {

  def create(path: Path): IndexedAppendableFileStore = {
    logger.info(s"Creating IndexedAppendableFileStore($path)")
    val afs = TypedAsyncFileStore.create[State](path)
    new IndexedAppendableFileStore(afs, path, 0)
  }

  def open(path: Path, initIndex: Int): IndexedAppendableFileStore = {
    logger.info(s"Opening IndexedAppendableFileStore($path)")
    val afs = TypedAsyncFileStore.open[State](path)
    new IndexedAppendableFileStore(afs, path, initIndex)
  }

  case class State(idx: Int, pos: Long) {
    def add(len: Int): State = State(idx + 1, pos + len)
  }
}

class IndexedAppendableFileStore(afs: TypedAsyncFileStore[IndexedAppendableFileStore.State], val path: Path, initIndex: Int) {
  import IndexedAppendableFileStore._

  private val state = new AtomicReference[State](State(initIndex, afs.aio.size()))

  def put(buffer: ByteBuffer): Future[State] = {
    val len = buffer.remaining()

    var curr: State = null
    var next: State = null
    do {
      curr = state.get()
      next = curr.add(len)
    } while (!state.compareAndSet(curr, next))

    afs.put(curr.pos, buffer, curr)
  }

  def get(pos: Long, len: Int): Future[ByteBuffer] = afs.get(pos, len)

  def current: Long = state.get().pos
  def commit(): Unit = afs.commit()
  def close(): Unit = afs.close()
}

