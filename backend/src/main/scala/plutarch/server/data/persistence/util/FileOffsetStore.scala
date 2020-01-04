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

import com.typesafe.scalalogging.LazyLogging
import plutarch.shared.collection.RangeMap
import sun.nio.ch.DirectBuffer

object FileOffsetStore extends LazyLogging {
  val BLOCK_SIZE = 8192
  val HEADER_SIZE: Int = 8192

  def create(path: Path, step: Long, count: Int): FileOffsetStore = {

    logger.info(s"Creating FileOffsetStore($path)")

    val channel = FileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE_NEW)

    val buffer = ByteBuffer.allocate(HEADER_SIZE)
    buffer.putLong(step)
    buffer.putLong(count)
    buffer.putLong(0L)
    buffer.putLong(0L)
    buffer.putLong(0L)
    buffer.putLong(count)
    buffer.flip()
    channel.write(buffer)

    new FileOffsetStore(channel, path)

  }

  def open(path: Path): FileOffsetStore = {

    logger.info(s"Opening FileOffsetStore($path)")

    val channel = FileChannel.open(
      path,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE)

    new FileOffsetStore(channel, path)
  }

}

class FileOffsetStore(channel: FileChannel, val path: Path) extends RangeMap with LazyLogging {

  import FileOffsetStore._

  private val head = channel.map(FileChannel.MapMode.READ_WRITE, 0, HEADER_SIZE)

  private val allocator = LongVariable.getAllocator(8)

  val stepVar = LongVariable(head, allocator.next())
  val countVar = LongVariable(head, allocator.next())
  val posVar = LongVariable(head, allocator.next())
  val firstVar = LongVariable(head, allocator.next())
  val currentVar = LongVariable(head, allocator.next())
  val remainingVar = LongVariable(head, allocator.next())

  logger.debug(
    s"FileOffsetStore($path) with header: " +
      s"stepVar.get=${stepVar.get}, " +
      s"countVar.get=${countVar.get}, " +
      s"posVar.get=${posVar.get}, " +
      s"firstVar.get=${firstVar.get}, " +
      s"currentVar.get=${currentVar.get}, " +
      s"remainingVar.get=${remainingVar.get}")

  val step: Long = stepVar.get
  val count: Long = countVar.get

  // stepVar.get=1000,
  // countVar.get=1048576,
  // posVar.get=3704112, 926028   925186
  // firstVar.get=1525122000000,
  // currentVar.get=1526047185000,
  // remainingVar.get=122548
  // todo!!!
  assert(posVar.get == 0 || posVar.get / 4 == (1 + (currentVar.get - firstVar.get) / stepVar.get))

  private val bodySize: Int = ((count * 4).toDouble / BLOCK_SIZE).ceil.toInt * BLOCK_SIZE
  private val body = channel.map(FileChannel.MapMode.READ_WRITE, HEADER_SIZE, bodySize)
  body.position(posVar.get.toInt)

  override def remaining: Int = Int.MaxValue

  def add(key: Long, offset: Int): Unit = {
    if (remainingVar.get > 0) {
      if (posVar.get == 0) {
        firstVar.set(key)
      } else if (key != currentVar.get + step) {
        throw new RuntimeException(s"Skipped offset key=$key != current=${currentVar.get} + step=$step")
      }
      currentVar.set(key)
      posVar.add(4)
      body.putInt(offset)
      remainingVar.add(-1)
    } else {
      throw new RuntimeException(s"Store FileOffsetStore($path) has no free slots, count=$count")
    }
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

  def isEmpty: Boolean = posVar.get == 0L

  def commit(): Unit = {
    channel.force(true)
  }

  // unsafe!
  def close(): Unit = {
    head.asInstanceOf[DirectBuffer].cleaner.clean()
    body.asInstanceOf[DirectBuffer].cleaner.clean()
    channel.close()
  }

}