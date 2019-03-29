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

package plutarch.server.pipelines

import akka.actor.ActorSystem
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations
import plutarch.shared.data.metrics.Conf

class ExternalPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {

  import Aggregations._

  val conf: Conf = Conf(
    name = "External",
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Sum, Min, Max, Count, Avg,
      //PercentileCont(0.01),
      PercentileCont(0.05),
      //PercentileCont(0.10),
      //PercentileCont(0.25),
      PercentileCont(0.50),
      //PercentileCont(0.75),
      //PercentileCont(0.90),
      PercentileCont(0.95),
      //PercentileCont(0.99),
      Stdev,
      First,
      Last),
    withTotal = false)

  val metric: Metric = metricManager.getOrCreate(conf)

}
