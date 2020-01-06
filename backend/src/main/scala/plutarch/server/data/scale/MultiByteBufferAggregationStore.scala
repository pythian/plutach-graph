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
import java.util
import java.util.concurrent.locks.ReentrantReadWriteLock
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.report.{ MultiByteBufferAggregationStoreReport, PageReport }
import plutarch.shared.collection.{ ByteRangeMap, Destroyer }
import scala.concurrent.Future
import scala.collection.JavaConverters._

object MultiByteBufferAggregationStore {
  val emptyByteBuffer: ByteBuffer = ByteBuffer.wrap(Array[Byte]())
  def create(step: Long, headerBaseSize: Int, storeBaseSize: Int, minOffsetsSize: Int = 1024 * 1024, minByteBufferSize: Int = 8 * 1024 * 1024): MultiByteBufferAggregationStore =
    new MultiByteBufferAggregationStore(step, headerBaseSize, storeBaseSize, minOffsetsSize, minByteBufferSize)

  private case class State(firstKey: Long = Long.MaxValue, currKey: Long = Long.MinValue, currOffset: Int = 0)

  case class Page(step: Long, headerBaseSize: Int, storeBaseSize: Int, minOffsetsSize: Int, minByteBufferSize: Int) {
    @volatile private var state = State()
    private val offsetsSize = minOffsetsSize.max(headerBaseSize / (step / 1000L).toInt)
    private val byteBufferSize = minByteBufferSize.max(storeBaseSize / (step / 1000L).toInt)
    private val offsets = new ByteRangeMap(step, offsetsSize)
    private val byteBuffer = ByteBuffer.allocateDirect(byteBufferSize)

    def add(key: Long, value: ByteBuffer): Boolean = {
      if (offsets.remaining < 4 || byteBuffer.remaining() < value.remaining()) {
        return false
      }
      val newCurrentOffset = state.currOffset + value.limit()
      assert(state.currKey == Long.MinValue || key == state.currKey + step)
      offsets.add(key, state.currOffset)
      if (value != emptyByteBuffer) {
        byteBuffer.put(value)
      }
      val newFirstKey = if (state.firstKey == Long.MaxValue) key else state.firstKey
      state = State(newFirstKey, key, newCurrentOffset)
      true
    }

    def get(x: Long, y: Long): ByteBuffer = {
      val thisState = state
      if (x <= thisState.currKey && y >= thisState.firstKey) {
        val xRound = x.max(thisState.firstKey)
        val yRound = y.min(thisState.currKey + step)
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

    def close(): Unit = {
      state = null
      offsets.close()
      Destroyer.destroy(byteBuffer)
    }

    def report: PageReport = {
      PageReport(offsets.capacity, byteBuffer.capacity())
    }

  }
}

class MultiByteBufferAggregationStore(step: Long, headerBaseSize: Int, storeBaseSize: Int, minOffsetsSize: Int, minByteBufferSize: Int) extends AggregationStore with LazyLogging {
  import MultiByteBufferAggregationStore._

  private var isClosed = false

  @volatile private var state = State()

  @volatile private var page = Page(step, headerBaseSize, storeBaseSize, minOffsetsSize, minByteBufferSize)
  private val pages: util.TreeMap[Long, Page] = {
    val map = new util.TreeMap[Long, Page]()
    map.put(Long.MinValue, page)
    map
  }
  val pagesLock = new ReentrantReadWriteLock()

  // for testing only
  private[scale] def getPages = {
    pages
  }

  private def addNext(key: Long, value: ByteBuffer): Unit = {
    if (!page.add(key, value)) {
      page = Page(step, headerBaseSize, storeBaseSize, minOffsetsSize, minByteBufferSize)
      assert(page.add(key, value), "Unable to add value to a new page")
      try {
        pagesLock.writeLock().lock()
        pages.put(key, page)
      } finally {
        pagesLock.writeLock().unlock()
      }
    }
  }

  def add(key: Long, value: ByteBuffer): Future[Unit] = {
    if (isClosed) {
      throw new RuntimeException(s"Trying to call checkState on closed MultiByteBufferAggregationStore")
    }
    addSync(key, value)
    Future.successful()
  }

  private def addSync(key: Long, value: ByteBuffer): Unit = {
    if (state.currKey == Long.MinValue || key == state.currKey + step) {
      addNext(key, value)
    } else if (key > state.currKey + step) {
      val keys = (state.currKey + step).until(key, step)
      for (key ← keys) {
        addNext(key, emptyByteBuffer)
      }
      addNext(key, value)
    } else {
      throw new Exception(s"Late data keys are not supported, key=$key, state.currKey=${state.currKey}")
    }
    val newFirstKey = if (state.firstKey == Long.MaxValue) key else state.firstKey
    state = State(newFirstKey, key)
  }

  def get(x: Long, y: Long): Future[ByteBuffer] = {
    Future.successful(getSync(x, y))
  }

  private[scale] def getSync(x: Long, y: Long): ByteBuffer = {
    val thisState = state
    if (x <= thisState.currKey && y >= thisState.firstKey) {

      // round x/y to align with offsets
      val xRound = ((x / step) * step).max(thisState.firstKey)
      val yRound = ((y / step) * step).min(thisState.currKey + step)

      val intersected = try {
        pagesLock.readLock().lock()
        val left = pages.floorKey(xRound)
        var right = pages.ceilingKey(yRound + step)
        if (right < yRound + step) {
          right = Long.MaxValue
        }
        pages.subMap(left, right).values().asScala.toList
      } finally {
        pagesLock.readLock().unlock()
      }

      if (intersected.size == 1) {
        intersected.head.get(xRound, yRound)
      } else {
        var len = 0
        val list = new util.ArrayList[ByteBuffer]()
        for (page ← intersected) {
          val bb = page.get(xRound, yRound)
          list.add(bb)
          len += bb.remaining()
        }
        val res = ByteBuffer.allocate(len)
        for (bb ← list.asScala) {
          res.put(bb)
        }
        res.rewind()
        res
      }

    } else {
      emptyByteBuffer
    }
  }

  override def close(): Unit = {
    isClosed = true
    state = null
    try {
      pagesLock.writeLock().lock()
      for (page ← pages.values().asScala) {
        page.close()
      }
    } finally {
      pagesLock.writeLock().unlock()
    }
  }

  def report: MultiByteBufferAggregationStoreReport = {
    MultiByteBufferAggregationStoreReport(pages.asScala.toList.map(p ⇒ p._1 -> p._2.report).toMap)
  }

}
