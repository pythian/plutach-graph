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
