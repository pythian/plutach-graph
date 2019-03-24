package plutarch.server.data.persistence.util

import java.nio.ByteBuffer
import java.nio.channels.{ AsynchronousFileChannel, CompletionHandler }
import java.nio.file.{ Path, StandardOpenOption }
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.{ Future, Promise }

object TypedAsyncFileStore extends LazyLogging {

  def create[T](path: Path): TypedAsyncFileStore[T] = {
    logger.info(s"Creating TypedAsyncFileStore($path)")
    val aio = AsynchronousFileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE_NEW)
    new TypedAsyncFileStore[T](aio, path)
  }

  def open[T](path: Path): TypedAsyncFileStore[T] = {
    logger.info(s"Opening TypedAsyncFileStore($path)")
    val aio = AsynchronousFileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE)
    new TypedAsyncFileStore[T](aio, path)
  }

}

class TypedAsyncFileStore[T](val aio: AsynchronousFileChannel, val path: Path) {

  case class WriteContext(buffer: ByteBuffer, len: Int, promise: Promise[T], content: T)
  case class ReadContext(buffer: ByteBuffer, pos: Long, len: Int, promise: Promise[ByteBuffer], exact: Boolean)

  private val writeCallback = new CompletionHandler[Integer, WriteContext] {
    override def completed(result: Integer, attachment: WriteContext): Unit = {
      if (result == attachment.len) {
        attachment.promise.success(attachment.content)
      } else {
        attachment.promise.failure(new RuntimeException(s"Failed to write with aio: length=${attachment.len}, result=$result"))
      }
    }
    override def failed(exc: Throwable, attachment: WriteContext): Unit = {
      attachment.promise.failure(exc)
    }
  }

  private val readCallback: CompletionHandler[Integer, ReadContext] = new CompletionHandler[Integer, ReadContext] {
    override def completed(result: Integer, attachment: ReadContext): Unit = {
      if (result == attachment.len || (!attachment.exact && result > -1)) {
        attachment.buffer.flip()
        attachment.promise.success(attachment.buffer)
      } else {
        attachment.promise.failure(new RuntimeException(s"Failed to read with aio: length=${attachment.len}, result=$result"))
      }
    }
    override def failed(exc: Throwable, attachment: ReadContext): Unit = {
      attachment.promise.failure(exc)
    }
  }

  def put(pos: Long, buffer: ByteBuffer, attachment: T): Future[T] = {
    val len = buffer.remaining()
    val p = Promise[T]()
    val ctx = WriteContext(buffer, len, p, attachment)
    aio.write(buffer, pos, ctx, writeCallback)
    p.future
  }

  def get(pos: Long, len: Int): Future[ByteBuffer] = {
    val buffer = ByteBuffer.allocateDirect(len)
    val p = Promise[ByteBuffer]()
    val ctx = ReadContext(buffer, pos, len, p, true)
    aio.read(buffer, pos, ctx, readCallback)
    p.future
  }

  def get2(pos: Long, len: Int): Future[ByteBuffer] = {
    val buffer = ByteBuffer.allocateDirect(len)
    val p = Promise[ByteBuffer]()
    val ctx = ReadContext(buffer, pos, len, p, false)
    aio.read(buffer, pos, ctx, readCallback)
    p.future
  }

  def commit(): Unit = {
    aio.force(true)
  }

  def close(): Unit = {
    aio.force(true)
    aio.close()
  }
}

