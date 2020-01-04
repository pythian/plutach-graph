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
import com.google.cloud.pubsub.v1.Subscriber
import com.google.pubsub.v1.ProjectSubscriptionName
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.store
import plutarch.server.data.store.{ DefaultMetricStoreCreatorCreator, MetricStoreCreator }
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations.{ Max, Min, Sum }
import plutarch.shared.data.metrics.Conf
import scala.collection.mutable
import plutarch.server.pipelines.spark.{ PubSubConnection, PubSubConnectionImpl, SparkPipeline, SparkPipelineImpl }

object SparkPipeline {

  val BUFFER_MAX_KEEP_TIME = 30000

  var system: ActorSystem = _
  var metricManager: MetricManager = _
  var webSocketFlowCoordinator: WebSocketFlowCoordinator = _
  val subscribers = mutable.Map.empty[String, Subscriber]

  def init(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem): Unit = {
    this.metricManager = metricManager
    this.system = system
    this.webSocketFlowCoordinator = webSocketFlowCoordinator
  }

  def getConf(name: String): Conf = Conf(
    name = name,
    //step = 60000,
    step = 10000,
    //step = 1000,
    //scales = Seq(60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24).map(_ / 60),
    scales = Seq(10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24).map(_ / 10),
    //scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Max, Min, Sum),
    withTotal = false)

  def create(name: String): SparkPipeline = {
    create(name, MetricStoreCreator.newConf(name))
  }

  def create(name: String, headerBaseSize: Int, storeBaseSize: Int): SparkPipeline = {
    val storeConf = new store.MetricStoreCreator.Conf(name, Map(
      DefaultMetricStoreCreatorCreator.HEADER_BASE_SIZE -> headerBaseSize,
      DefaultMetricStoreCreatorCreator.STORE_BASE_SIZE -> storeBaseSize))
    create(name, storeConf)
  }

  def create(name: String, storeConf: MetricStoreCreator.Conf): SparkPipeline =
    new SparkPipelineImpl(
      spark.Config.default,
      getConf(name),
      storeConf)(system, metricManager)

  def dropMetric(name: String): Unit = {
    metricManager.drop(name)
  }

  def connectSubscription(
    projectId:    String,
    subscription: String,
    metrics:      Seq[String],
    parseFn:      String ⇒ Option[(String, Long, Seq[(String, Double)])]): PubSubConnection = {
    val subscriptionName = ProjectSubscriptionName.of(projectId, subscription)
    if (subscribers.contains(subscriptionName.toString)) {
      throw new RuntimeException("Subscription is already subscribed")
    }
    val connection = new PubSubConnectionImpl(
      PubSubConnectionImpl.Config(BUFFER_MAX_KEEP_TIME),
      subscriptionName,
      metrics,
      parseFn)(system, metricManager, webSocketFlowCoordinator)
    subscribers.put(subscriptionName.toString, connection.subscriber)
    connection
  }

  def dropSubscriber(projectId: String, subscription: String): Unit = {
    val subscriptionName = ProjectSubscriptionName.of(projectId, subscription)
    subscribers.get(subscriptionName.toString).foreach(_.stopAsync().awaitTerminated())
    subscribers -= subscriptionName.toString
  }

}