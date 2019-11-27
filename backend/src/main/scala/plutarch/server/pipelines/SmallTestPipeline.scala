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
import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations
import plutarch.shared.data.metrics.Conf
import scala.concurrent.{ Await, Future }

class SmallTestPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {

  import system.dispatcher
  import Aggregations._

  val conf: Conf = Conf(
    name = "test",
    step = 1000,
    scales = Seq(1),
    aggregations = Seq(Sum),
    withTotal = true)

  val metric: Metric = metricManager.getOrCreate(conf)

  def publish(t: Long, data: Seq[(String, Double)], silent: Boolean = false): Unit = {
    Await.ready(metric.add(t, data), (600 * 1000) millis)
  }

  def init(): Unit = {
    val t0 = (System.currentTimeMillis() / 1000) * 1000

    publish(t0 - 3000, Seq(("A", 1.0)))
    publish(t0 - 2000, Seq(("B", 1.0)))
    publish(t0 - 1000, Seq()) // last message is current, so we either have to publish is to websocket or explicitly "close"

  }

  init()

}