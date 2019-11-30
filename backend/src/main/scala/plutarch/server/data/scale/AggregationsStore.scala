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

import scala.concurrent.{ ExecutionContext, Future }
import com.typesafe.scalalogging.LazyLogging
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.server.data.scale.Scale.CurrentR
import plutarch.server.data.store.AggregationStoreCreator

trait AggregationsStore {
  def add(curr: CurrentR)(implicit executor: ExecutionContext): Future[Unit]
  def get(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
  def close(): Unit
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
    override def close(): Unit = {
      aggregationStores.foreach(_._2.close())
    }
  }
}