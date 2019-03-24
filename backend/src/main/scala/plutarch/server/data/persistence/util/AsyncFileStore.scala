package plutarch.server.data.persistence.util

import java.nio.ByteBuffer
import java.nio.channels.{ AsynchronousFileChannel, CompletionHandler }
import java.nio.file.{ Path, StandardOpenOption }
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.{ Future, Promise }

object AsyncFileStore extends LazyLogging {
  case class AioContext[T](buffer: ByteBuffer, pos: Long, len: Int, promise: Promise[T], exact: Boolean)

  def create(path: Path): AsyncFileStore = {
    logger.info(s"Creating AsyncFileStore($path)")
    val aio = AsynchronousFileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE_NEW)
    new AsyncFileStore(aio, path)
  }

  def open(path: Path): AsyncFileStore = {
    logger.info(s"Opening AsyncFileStore($path)")
    val aio = AsynchronousFileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE)
    new AsyncFileStore(aio, path)
  }

  private val writeCallback = new CompletionHandler[Integer, AioContext[Long]] {
    override def completed(result: Integer, attachment: AioContext[Long]): Unit = {
      if (result == attachment.len || (!attachment.exact && result > -1)) {
        attachment.promise.success(attachment.pos)
      } else {
        attachment.promise.failure(new RuntimeException(s"Failed to write with aio: length=${attachment.len}, result=$result"))
      }
    }
    override def failed(exc: Throwable, attachment: AioContext[Long]): Unit = {
      attachment.promise.failure(exc)
    }
  }

  private val readCallback: CompletionHandler[Integer, AioContext[ByteBuffer]] = new CompletionHandler[Integer, AioContext[ByteBuffer]] {
    override def completed(result: Integer, attachment: AioContext[ByteBuffer]): Unit = {
      if (result == attachment.len || (!attachment.exact && result > -1)) {
        attachment.buffer.flip()
        attachment.promise.success(attachment.buffer)
      } else {
        attachment.promise.failure(new RuntimeException(s"Failed to read with aio: length=${attachment.len}, result=$result"))
      }
    }
    override def failed(exc: Throwable, attachment: AioContext[ByteBuffer]): Unit = {
      attachment.promise.failure(exc)
    }
  }
}

class AsyncFileStore(val aio: AsynchronousFileChannel, val path: Path) {
  import AsyncFileStore._

  def put(pos: Long, buffer: ByteBuffer): Future[Long] = {
    val len = buffer.remaining()
    val p = Promise[Long]()
    val ctx = AioContext(buffer, pos, len, p, true)
    aio.write(buffer, pos, ctx, writeCallback)
    p.future
  }

  def get(pos: Long, len: Int): Future[ByteBuffer] = {
    val buffer = ByteBuffer.allocateDirect(len)
    val p = Promise[ByteBuffer]()
    val ctx = AioContext(buffer, pos, len, p, true)
    aio.read(buffer, pos, ctx, readCallback)
    p.future
  }

  def get2(pos: Long, len: Int): Future[ByteBuffer] = {
    val buffer = ByteBuffer.allocateDirect(len)
    val p = Promise[ByteBuffer]()
    val ctx = AioContext(buffer, pos, len, p, false)
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

