package plutarch.server.data.scale

import scala.collection.mutable.{ HashMap ⇒ MHashMap }
import java.nio.ByteBuffer

import boopickle.Default.Pickle
import plutarch.server.data.accumulators.{ CombinedAccumulator, CombinedAccumulatorCreator }

import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.store.AggregationStoreCreator
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers

trait Scale {
  def add(t: Long, values: Seq[(Int, Double)])(implicit executor: ExecutionContext): Future[Unit]
  def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
  def get(aggregation: Aggregation, intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[Seq[ByteBuffer]]
  def getCurrent(aggregation: Aggregation): (Long, Long, Seq[(Int, Any)])
  def keyRoundToStep(t: Long): Long
}

object Scale extends LazyLogging {
  val totalId = 0

  trait CurrentR {
    def key: Long
    def get(aggregation: Aggregation): ByteBuffer
  }

  def create(name: String, scale: Int, step: Long, accCreator: CombinedAccumulatorCreator, storeCreator: AggregationStoreCreator, withTotal: Boolean): Scale =
    new Impl(name, scale, step, accCreator, storeCreator, withTotal)

  class Impl(val name: String, val scale: Int, val step: Long, val accCreator: CombinedAccumulatorCreator, val storeCreator: AggregationStoreCreator, withTotal: Boolean) extends Scale {

    override def toString: String = s"Scale($name, $scale, $step)"

    private case class Current(key: Long) extends CurrentR {
      private val data = MHashMap.empty[Int, CombinedAccumulator]
      private var version = 0L
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

  }

}