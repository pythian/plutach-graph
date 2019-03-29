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
import scala.concurrent.{ ExecutionContext, Future }

object FileClassStore {

  trait SerDe[T] {
    def ser(t: T): ByteBuffer
    def de(buffer: ByteBuffer): T
  }

  def create[T](path: Path, size: Int, serDe: SerDe[T]): FileClassStore[T] = {
    val afs = AsyncFileStore.create(path)
    new FileClassStore[T](afs, size, serDe)
  }

  def open[T](path: Path, size: Int, serDe: SerDe[T]): FileClassStore[T] = {
    val afs = AsyncFileStore.open(path)
    new FileClassStore[T](afs, size, serDe)
  }

}

class FileClassStore[T](afs: AsyncFileStore, size: Int, serDe: FileClassStore.SerDe[T]) {

  def set(index: Int, value: T): Future[Long] = {
    val pos = index * size
    val data = serDe.ser(value)
    afs.put(pos, data)
  }

  def get(index: Int)(implicit executionContext: ExecutionContext): Future[T] = {
    val pos = index * size
    afs.get2(pos, size).map(serDe.de)
  }

  def commit(): Unit = afs.commit()
  def close(): Unit = afs.close()

}
