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

import scala.concurrent.duration._
import scala.collection.mutable.{ HashMap ⇒ MHashMap }
import java.nio.ByteBuffer
import boopickle.Default.Pickle
import plutarch.server.data.accumulators.{ CombinedAccumulator, CombinedAccumulatorCreator }
import scala.concurrent.{ Await, ExecutionContext, Future }
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.report.ScaleReport
import plutarch.server.data.store.AggregationStoreCreator
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers

trait Scale {
  def add(t: Long, values: Seq[(Int, Double)])(implicit executor: ExecutionContext): Future[Unit]
  def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
  def get(aggregation: Aggregation, intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[Seq[ByteBuffer]]
  def getCurrent(aggregation: Aggregation): (Long, Long, Seq[(Int, Any)])
  def keyRoundToStep(t: Long): Long
  def freeze()(implicit executor: ExecutionContext): Unit
  def close(): Unit
  def report: ScaleReport
}

// TODO better error
// TODO buffered aggregation store to have better buffering
object Scale extends LazyLogging {
  val totalId = 0
  private val timeout = 10 seconds

  trait CurrentR {
    def key: Long
    def get(aggregation: Aggregation): ByteBuffer
  }

  def create(name: String, scale: Int, step: Long, accCreator: CombinedAccumulatorCreator, storeCreator: AggregationStoreCreator, withTotal: Boolean): Scale =
    new Impl(name, scale, step, accCreator, storeCreator, withTotal)

  class Impl(val name: String, val scale: Int, val step: Long, val accCreator: CombinedAccumulatorCreator, val storeCreator: AggregationStoreCreator, withTotal: Boolean) extends Scale {

    private var isClose = false
    private var frozen = false

    override def toString: String = s"Scale($name, $scale, $step)"

    private case class Current(key: Long) extends CurrentR {
      private val data = MHashMap.empty[Int, CombinedAccumulator]
      private var version = 0L
      def getObjectIds: Seq[Int] = data.keySet.toSeq
      def add(t: Long, values: Seq[(Int, Double)]): Unit = {
        version += 1
        for ((objId, value) ← values) {
          data.getOrElseUpdate(objId, accCreator.create()).add(t, value)
          if (withTotal) {
            data.getOrElseUpdate(totalId, accCreator.create()).add(t, value)
          }
        }
      }
      def get(aggregation: Aggregation): ByteBuffer = {
        val aggData = data.iterator.map(x ⇒ x._1 -> x._2.get(aggregation))
        import Picklers.AggDataPickler
        Pickle.intoBytes(new Picklers.AggData(aggregation, key, data.size, aggData))
      }
      def getDe(aggregation: Aggregation): (Long, Long, Seq[(Int, Any)]) = {
        (key, version, data.iterator.map(x ⇒ x._1 -> x._2.get(aggregation)).toList)
      }
    }

    private val thisStep = scale * step
    def keyRoundToStep(t: Long): Long = thisStep * (t / thisStep)

    // this must be immutable as concurrent queries must be supported!
    @volatile private var curr = Current(Long.MinValue)

    val aggregationsStore: AggregationsStore = AggregationsStore.create(thisStep, accCreator.getAggregations, storeCreator)

    def add(t: Long, values: Seq[(Int, Double)])(implicit executor: ExecutionContext): Future[Unit] = {
      if (isClose) {
        throw new RuntimeException(s"Trying to call checkState on freezed Scale")
      }
      if (frozen) {
        throw new RuntimeException(s"Trying to call checkState on frozen Scale")
      }
      val thisKey = keyRoundToStep(t)
      //logger.debug(s"${this.toString} received t=$t, curr.key=${curr.key}, thisKey=$thisKey, thisKey-curr.key=${thisKey - curr.key}")
      if (thisKey == curr.key) {
        // this point, add to acc
        curr.add(t, values)
        Future.successful()
      } else if (thisKey > curr.key) {
        val future = if (curr.key > Long.MinValue) {
          aggregationsStore.add(curr)
        } else {
          Future.successful()
        }
        curr = Current(thisKey)
        curr.add(t, values)
        future
      } else {
        // old key, update or ignore?
        throw new Exception("Old key not supported: todo make keeping few recent accs")
      }
    }

    override def freeze()(implicit executor: ExecutionContext): Unit = {
      frozen = true
      val future = if (curr.key > Long.MinValue) {
        aggregationsStore.add(curr)
      } else {
        Future.successful()
      }
      Await.result(future, timeout)
    }

    def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      aggregationsStore.get(aggregation, x, y)
    }

    def get(aggregation: Aggregation, intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[Seq[ByteBuffer]] = {
      Future.sequence(
        intervals.map {
          case (x, y) ⇒
            aggregationsStore.get(aggregation, x, y)
        })
    }

    def getCurrent(aggregation: Aggregation): (Long, Long, Seq[(Int, Any)]) = {
      curr.getDe(aggregation)
    }

    override def close(): Unit = {
      isClose = true
      curr = null
      aggregationsStore.close()
    }

    def report: ScaleReport = {
      ScaleReport(aggregationsStore.report)
    }

  }

}