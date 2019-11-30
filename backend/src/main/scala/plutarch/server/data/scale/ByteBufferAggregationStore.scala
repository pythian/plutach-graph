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

package plutarch.server.data.scale

import java.nio.ByteBuffer

import com.typesafe.scalalogging.LazyLogging
import plutarch.shared.collection.{ ByteRangeMap, Destroyer }

import scala.concurrent.{ ExecutionContext, Future }

object ByteBufferAggregationStore {
  val emptyByteBuffer: ByteBuffer = ByteBuffer.wrap(Array[Byte]())
  def create(step: Long, headerBaseSize: Int, storeBaseSize: Int): ByteBufferAggregationStore =
    new ByteBufferAggregationStore(step, headerBaseSize, storeBaseSize)
}

class ByteBufferAggregationStore(step: Long, headerBaseSize: Int, storeBaseSize: Int) extends AggregationStore with LazyLogging {
  import ByteBufferAggregationStore._

  private var isClosed = false

  private case class State(firstKey: Long = Long.MaxValue, currKey: Long = Long.MinValue, currOffset: Int = 0)
  @volatile private var state = State()

  private val offsets = new ByteRangeMap(step, headerBaseSize / (step / 1000L).toInt)
  private val byteBuffer = ByteBuffer.allocateDirect(storeBaseSize / (step / 1000L).toInt)

  def add(key: Long, value: ByteBuffer): Future[Unit] = {
    if (isClosed) {
      throw new RuntimeException(s"Trying to call checkState on closed ByteBufferAggregationStore")
    }
    //logger.debug(s"ByteBufferAggregationStore($step) recived key=$key, state.currKey=${state.currKey}")

    val newCurrentOffset = state.currOffset + value.limit()
    if (state.currKey == Long.MinValue || key == state.currKey + step) {
      offsets.add(key, state.currOffset)
    } else if (key > state.currKey + step) {
      val keys = (state.currKey + step).to(key, step)
      for (key ← keys) {
        offsets.add(key, state.currOffset)
      }
    } else {
      throw new Exception(s"Late data keys are not supported, key=$key, state.currKey=${state.currKey}")
    }
    byteBuffer.put(value)
    val newFirstKey = if (state.firstKey == Long.MaxValue) key else state.firstKey
    state = State(newFirstKey, key, newCurrentOffset)
    Future.successful()
  }

  def get(x: Long, y: Long): Future[ByteBuffer] = Future.successful {
    val thisState = state
    if (x <= thisState.currKey && y >= thisState.firstKey) {

      // round x/y to align with offsets
      val xRound = ((x / step) * step).max(thisState.firstKey)
      val yRound = ((y / step) * step).min(thisState.currKey + step)

      val startOffset = offsets.getOrElse(xRound, 0)
      val endOffset = offsets.getOrElse(yRound, state.currOffset)
      val src = byteBuffer.asReadOnlyBuffer()
      src.position(startOffset)
      src.limit(endOffset)
      src
    } else {
      emptyByteBuffer
    }
  }

  override def close(): Unit = {
    isClosed = true
    state = null
    offsets.close()
    Destroyer.destroy(byteBuffer)
  }
}
