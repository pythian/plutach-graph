package plutarch.server.data.scale

import java.nio.ByteBuffer

import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.store.AggregationStoreCreator
import plutarch.shared.data.Aggregations.Aggregation

trait AggregationStore {
  def add(key: Long, value: ByteBuffer): Future[Unit]
  def get(x: Long, y: Long): Future[ByteBuffer]
}

object AggregationStore extends LazyLogging {
  def create(step: Long, aggregation: Aggregation, storeCreator: AggregationStoreCreator): AggregationStore =
    storeCreator.createAggregationStore(step, aggregation)
}