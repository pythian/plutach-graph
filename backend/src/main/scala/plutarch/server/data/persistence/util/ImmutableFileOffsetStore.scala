package plutarch.server.data.persistence.util

import java.nio.channels.FileChannel
import java.nio.file.{ Path, Paths, StandardOpenOption }
import java.nio.{ ByteBuffer, MappedByteBuffer }

import com.typesafe.scalalogging.LazyLogging
import plutarch.shared.collection.RangeMap
import sun.nio.ch.DirectBuffer

object ImmutableFileOffsetStore extends LazyLogging {
  val BLOCK_SIZE = 8192
  val HEADER_SIZE: Int = 8192

  def create(rangeMap: RangeMap, path: String): ImmutableFileOffsetStore = {
    create(rangeMap, Paths.get(path))
  }

  def create(rangeMap: RangeMap, path: Path): ImmutableFileOffsetStore = {

    logger.info(s"Creating ImmutableFileOffsetStore($path)")

    val channel = FileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE_NEW)

    val count = ((rangeMap.until - rangeMap.from) / rangeMap.step).toInt
    val pos = 4 * count
    val current = rangeMap.until - rangeMap.step

    val buffer = ByteBuffer.allocate(HEADER_SIZE)

    buffer.putLong(rangeMap.step)
    buffer.putLong(count)
    buffer.putLong(pos)
    buffer.putLong(rangeMap.from)
    buffer.putLong(current)
    buffer.flip()
    channel.write(buffer)

    channel.position(HEADER_SIZE)
    val rbuffer = rangeMap.getAsBuffer
    channel.write(rangeMap.getAsBuffer)

    val bodySize: Int = ((count * 4).toDouble / BLOCK_SIZE).ceil.toInt * BLOCK_SIZE
    val paddingSize = bodySize - rbuffer.limit()
    val padding = ByteBuffer.allocate(paddingSize)
    padding.limit(paddingSize)
    channel.write(padding)

    channel.force(true)

    new ImmutableFileOffsetStore(channel, path)
  }

  def open(path: Path): ImmutableFileOffsetStore = {

    logger.info(s"Opening ImmutableFileOffsetStore($path)")

    val channel = FileChannel.open(path, StandardOpenOption.READ)

    new ImmutableFileOffsetStore(channel, path)
  }
}

class ImmutableFileOffsetStore(channel: FileChannel, val path: Path) extends RangeMap with LazyLogging {

  import ImmutableFileOffsetStore._

  private val head: MappedByteBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, HEADER_SIZE)

  private val allocator = LongVariable.getAllocator(8)

  val stepVar = LongVariable(head, allocator.next())
  val countVar = LongVariable(head, allocator.next())
  val posVar = LongVariable(head, allocator.next())
  val firstVar = LongVariable(head, allocator.next())
  val currentVar = LongVariable(head, allocator.next())

  logger.debug(
    s"ImmutableFileOffsetStore($path) with header: " +
      s"stepVar.get=${stepVar.get}, " +
      s"countVar.get=${countVar.get}, " +
      s"posVar.get=${posVar.get}, " +
      s"firstVar.get=${firstVar.get}, " +
      s"currentVar.get=${currentVar.get}")

  assert(posVar.get == 0 || posVar.get / 4 == (1 + (currentVar.get - firstVar.get) / stepVar.get))

  private val bodySize: Int = ((countVar.get * 4).toDouble / BLOCK_SIZE).ceil.toInt * BLOCK_SIZE
  private val body = channel.map(FileChannel.MapMode.READ_ONLY, HEADER_SIZE, bodySize)

  val step: Long = stepVar.get

  def add(key: Long, offset: Int): Unit = {
    throw new RuntimeException("Trying to modify immutable store")
  }

  def getOrElse(key: Long, default: ⇒ Int): Int = {
    if (key < firstVar.get || key > currentVar.get) {
      default
    } else {
      val offset = 4 * ((key - firstVar.get) / step).toInt
      body.getInt(offset)
    }
  }

  def getAsBuffer: ByteBuffer = {
    val buff = body.asReadOnlyBuffer()
    buff.flip()
    buff
  }

  def from: Long = firstVar.get

  def until: Long = currentVar.get + step

  // unsafe!
  def close(): Unit = {
    head.asInstanceOf[DirectBuffer].cleaner.clean()
    body.asInstanceOf[DirectBuffer].cleaner.clean()
    channel.close()
  }
}
