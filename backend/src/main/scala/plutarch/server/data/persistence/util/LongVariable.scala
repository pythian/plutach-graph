package plutarch.server.data.persistence.util

import java.nio.{ ByteBuffer, ByteOrder }
import java.util.concurrent.atomic.AtomicLong

object LongVariable {

  def apply(buffer: ByteBuffer, pos: Int): LongVariable = new LongVariable(buffer, pos)

  def apply(buffer: ByteBuffer, pos: Int, init: Long): LongVariable = {
    val variable = new LongVariable(buffer, pos)
    variable.set(init)
    variable
  }

  def getAllocator(size: Int): Allocator = new Allocator(size)

  class Allocator(size: Int) {
    private var current = -size
    def next(): Int = {
      current += size
      current
    }
  }

}

class LongVariable(buffer: ByteBuffer, pos: Int) {
  //assert(buffer.order() == ByteOrder.BIG_ENDIAN)

  private val init = buffer.getLong(pos)
  private val inner = new AtomicLong(init)

  def set(value: Long): Unit = {
    buffer.putLong(pos, value)
    inner.set(value)
  }

  def get: Long = {
    inner.get()
  }

  def add(delta: Long): Unit = {
    val value = inner.addAndGet(delta)
    buffer.putLong(pos, value)
  }

}
