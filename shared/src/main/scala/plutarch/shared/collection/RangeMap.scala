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

package plutarch.shared.collection

import java.nio.ByteBuffer

// trait manages sequenced key-value storing of key = step*i + x0
trait RangeMap {
  def step: Long
  def hasRemaining: Boolean
  def add(key: Long, offset: Int): Unit
  def getOrElse(key: Long, default: ⇒ Int): Int
  def getAsBuffer: ByteBuffer
  def from: Long
  def until: Long
  def close(): Unit
}

// storing keys as buffer of ints
class ByteRangeMap(val step: Long, capacity: Int) extends RangeMap {
  private var first = Long.MinValue
  private var current = Long.MinValue
  private var offsets = ByteBuffer.allocateDirect(capacity)
  def hasRemaining: Boolean = {
    offsets.hasRemaining
  }
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
  override def close(): Unit = {
    Destroyer.destroy(offsets)
    offsets = null
  }
}