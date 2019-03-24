package plutarch.server.data.scale

import java.nio.ByteBuffer

import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.LazyLogging
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.server.data.scale.Scale.CurrentR
import plutarch.server.data.store.AggregationStoreCreator

trait AggregationsStore {
  def add(curr: CurrentR)(implicit executor: ExecutionContext): Future[Unit]
  def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
}

object AggregationsStore extends LazyLogging {
  def create(step: Long, aggregations: Seq[Aggregation], storeCreator: AggregationStoreCreator): AggregationsStore =
    new Impl(step, aggregations, storeCreator)

  private case class AggregationsStoreState(first: Long, last: Long)

  class Impl(step: Long, aggregations: Seq[Aggregation], storeCreator: AggregationStoreCreator) extends AggregationsStore {
    private val aggregationStores = aggregations.map(agg ⇒ agg -> AggregationStore.create(step, agg, storeCreator)).toMap
    def add(curr: CurrentR)(implicit executor: ExecutionContext): Future[Unit] = {
      val key = curr.key
      val futures = for {
        (agg, aggregationStore) ← aggregationStores
      } yield {
        val value = curr.get(agg)
        aggregationStore.add(key, value)
      }
      Future.sequence(futures).map(_ ⇒ ())
    }
    def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      aggregationStores(aggregation).get(x, y)
    }
  }
}