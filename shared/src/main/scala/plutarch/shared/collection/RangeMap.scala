package plutarch.shared.collection

import java.nio.ByteBuffer

// trait manages sequenced key-value storing of key = step*i + x0
trait RangeMap {
  def step: Long
  def add(key: Long, offset: Int): Unit
  def getOrElse(key: Long, default: ⇒ Int): Int
  def getAsBuffer: ByteBuffer
  def from: Long
  def until: Long
}

// storing keys as buffer of ints
class ByteRangeMap(val step: Long, capacity: Int) extends RangeMap {
  private var first = Long.MinValue
  private var current = Long.MinValue
  private val offsets = ByteBuffer.allocateDirect(capacity)
  def add(key: Long, offset: Int): Unit = {
    if (first == Long.MinValue) {
      first = key
    } else if (key != current + step) {
      throw new Exception(s"Skipped offset key=$key != current=$current + step=$step")
    }
    current = key
    offsets.putInt(offset)
  }
  def getOrElse(key: Long, default: ⇒ Int): Int = {
    if (key < first || key > current) {
      default
    } else {
      val offset = 4 * ((key - first) / step).toInt
      offsets.getInt(offset)
    }
  }
  def getAsBuffer: ByteBuffer = {
    val buff = offsets.asReadOnlyBuffer()
    buff.flip()
    buff
  }
  def from: Long = first
  def until: Long = current + step
}