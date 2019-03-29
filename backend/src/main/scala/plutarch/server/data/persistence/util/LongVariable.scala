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
