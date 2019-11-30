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

package plutarch.server.data.metricManager

import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metrics.Metric
import plutarch.server.data.store._
import plutarch.shared.data.metrics.Conf

/*
*
* Persist state
* load metrics from disk and other tasks
*
* */
class MetricManager(val storeCreatorCreator: MetricStoreCreatorCreator) extends LazyLogging {
  private var metrics = Map.empty[String, Metric]

  def getOrCreate(conf: Conf): Metric = {
    val storeConf = MetricStoreCreator.newConf(conf.name)
    getOrCreate(conf, storeConf)
  }

  def getOrCreate(conf: Conf, storeConf: MetricStoreCreator.Conf): Metric = this.synchronized {
    val storeCreator = storeCreatorCreator.create(storeConf)
    val metric: Metric = Metric.create(conf, storeCreator)
    metrics += (metric.name -> metric)
    logger.info("registered metric={}", metric.name)
    metric
  }

  def listNames: Seq[String] = metrics.keys.toSeq
  def getMetric(name: String): Option[Metric] = metrics.get(name)
  def drop(name: String): Unit = {
    metrics.get(name) match {
      case Some(x) ⇒
        metrics = metrics - name
        x.close()
      case None ⇒
    }
  }

  // todo get from persist or register new
  // todo runtime registration and creation via API
}

object MetricManager {
  def create(storeCreatorCreator: MetricStoreCreatorCreator): MetricManager = new MetricManager(storeCreatorCreator)
}