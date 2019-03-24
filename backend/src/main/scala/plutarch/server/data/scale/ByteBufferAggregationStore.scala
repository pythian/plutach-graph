package plutarch.server.data.scale

import java.nio.ByteBuffer
import plutarch.shared.collection.ByteRangeMap
import scala.concurrent.{ ExecutionContext, Future }

object ByteBufferAggregationStore {
  def rangeMapBufferSize(step: Long): Int = (64L * 1024L * 1024L * 1024L / step).toInt
  def storeBufferSize(step: Long): Int = (256L * 1024L * 1024L * 1024L / step).toInt
  val emptyByteBuffer: ByteBuffer = ByteBuffer.wrap(Array[Byte]())
  def create(step: Long): ByteBufferAggregationStore = new ByteBufferAggregationStore(step)
}

class ByteBufferAggregationStore(step: Long) extends AggregationStore {
  import ByteBufferAggregationStore._

  private case class State(firstKey: Long = Long.MaxValue, currKey: Long = Long.MinValue, currOffset: Int = 0)
  @volatile private var state = State()

  private val offsets = new ByteRangeMap(step, rangeMapBufferSize(step))
  private val byteBuffer = ByteBuffer.allocateDirect(storeBufferSize(step))

  def add(key: Long, value: ByteBuffer): Future[Unit] = {
    val newCurrentOffset = state.currOffset + value.limit()
    if (state.currKey == Long.MinValue || key == state.currKey + step) {
      offsets.add(key, state.currOffset)
    } else if (key > state.currKey + step) {
      val keys = (state.currKey + step).to(key, step)
      for (key ← keys) {
        offsets.add(key, state.currOffset)
      }
    } else {
      throw new Exception("Late data keys are not supported")
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
}
