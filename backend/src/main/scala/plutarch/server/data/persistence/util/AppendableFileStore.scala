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
