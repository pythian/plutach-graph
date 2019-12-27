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
import plutarch.server.data.scale.{ AggregationStore, MultiByteBufferAggregationStore }
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
    def getAs[T](key: String, default: T): T = map.getOrElse(key, default).asInstanceOf[T]
    def getAs[T](key: String): T = map(key).asInstanceOf[T]
    def set(key: String, value: Any): Conf = new Conf(name, map + (key -> value))
  }
  def newConf(name: String): Conf = new Conf(name, Map.empty)
}

trait MetricStoreCreatorCreator {
  def create(conf: MetricStoreCreator.Conf): MetricStoreCreator
}

object DefaultMetricStoreCreatorCreator extends MetricStoreCreatorCreator {
  val HEADER_BASE_SIZE = "headerBaseSize"
  val STORE_BASE_SIZE = "storeBaseSize"
  private val DEFAULT_HEADER_BASE_SIZE = 64 * 1024 * 1024
  private val DEFAULT_STORE_BASE_SIZE = 512 * 1024 * 1024

  private class Impl(conf: MetricStoreCreator.Conf) extends MetricStoreCreator {
    def createRawStore(): Raw = Raw.create(conf.name)
    def createAggregationStore(step: Long, aggregation: Aggregation): AggregationStore =
      MultiByteBufferAggregationStore.create(
        step,
        conf.getAs[Int](HEADER_BASE_SIZE, DEFAULT_HEADER_BASE_SIZE),
        conf.getAs[Int](STORE_BASE_SIZE, DEFAULT_STORE_BASE_SIZE))
    def createObjectsStore(): Objects = Objects.create(conf.name)
  }
  def create(conf: MetricStoreCreator.Conf): MetricStoreCreator = new Impl(conf)
}