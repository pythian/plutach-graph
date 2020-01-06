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
import plutarch.server.data.report.AggregationStoreReport
import plutarch.server.data.store.AggregationStoreCreator
import plutarch.shared.data.Aggregations.Aggregation

trait AggregationStore {
  def add(key: Long, value: ByteBuffer): Future[Unit]
  def get(x: Long, y: Long): Future[ByteBuffer]
  def close(): Unit
  def report: AggregationStoreReport
}

object AggregationStore extends LazyLogging {
  def create(step: Long, aggregation: Aggregation, storeCreator: AggregationStoreCreator): AggregationStore =
    storeCreator.createAggregationStore(step, aggregation)
}