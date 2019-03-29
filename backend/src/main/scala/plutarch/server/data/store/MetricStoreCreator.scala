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

package plutarch.server.data.store

import plutarch.server.data.objects.Objects
import plutarch.server.data.raw.Raw
import plutarch.server.data.scale.{ AggregationStore, ByteBufferAggregationStore }
import plutarch.shared.data.Aggregations.Aggregation

trait AggregationStoreCreator {
  def createAggregationStore(step: Long, aggregation: Aggregation): AggregationStore
}

trait RawStoreCreator {
  def createRawStore(): Raw
}

trait ObjectsStoreCreator {
  def createObjectsStore(): Objects
}

trait MetricStoreCreator extends AggregationStoreCreator with RawStoreCreator with ObjectsStoreCreator

object MetricStoreCreator {
  class Conf(val name: String, map: Map[String, Any]) {
    def getAs[T](key: String): T = map(key).asInstanceOf[T]
    def set(key: String, value: Any): Conf = new Conf(name, map + (key -> value))
  }
  def newConf(name: String): Conf = new Conf(name, Map.empty)
}

trait MetricStoreCreatorCreator {
  def create(conf: MetricStoreCreator.Conf): MetricStoreCreator
}

object DefaultMetricStoreCreatorCreator extends MetricStoreCreatorCreator {
  private class Impl(conf: MetricStoreCreator.Conf) extends MetricStoreCreator {
    def createRawStore(): Raw = Raw.create(conf.name)
    def createAggregationStore(step: Long, aggregation: Aggregation): AggregationStore = ByteBufferAggregationStore.create(step)
    def createObjectsStore(): Objects = Objects.create(conf.name)
  }
  def create(conf: MetricStoreCreator.Conf): MetricStoreCreator = new Impl(conf)
}