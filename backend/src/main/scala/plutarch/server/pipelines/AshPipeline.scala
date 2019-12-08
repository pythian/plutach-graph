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

import java.nio.file.{ Files, Path, Paths }

import akka.actor.ActorSystem
import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations
import plutarch.shared.data.metrics.Conf

import scala.concurrent.{ Await, Future }
import scala.io.Source

class AshPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {

  import system.dispatcher
  import Aggregations._

  private def getConf(name: String): Conf = Conf(
    name = name,
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Sum),
    withTotal = true)

  //  val confs: Seq[Conf] = Seq(
  //    getConf("sl_wait_class"),
  //    getConf("sl_event"),
  //    getConf("sl_type"),
  //    getConf("sl_sql_id"))

  val confs: Seq[Conf] = Seq(getConf("sl_event"))

  val metrics: Map[String, Metric] = confs.map(c ⇒ c.name -> metricManager.getOrCreate(c)).toMap

  def publish(name: String, t: Long, data: Seq[(String, Double)], silent: Boolean = false): Future[Unit] = {
    metrics(name).add(t, data)
  }

  def init(): Unit = {
    for (name ← metrics.keys) {
      val futures = for ((t, obj) ← read(name).sortBy(_._1)) yield {
        publish(name, t, Seq((obj, 1.0)))
      }
      Await.ready(Future.sequence(futures), (600 * 1000) millis)
      metrics(name).freeze()
      logger.info(s"Loaded metric $name")
    }
  }

  def read(name: String): Seq[(Long, String)] = {
    import scala.collection.JavaConverters._
    val base = s"../data/$name/"
    val files = Files.list(Paths.get(base)).iterator().asScala.toList
    logger.info(s"Files ${files.map(_.toUri).mkString("\n", "\n", "")}")
    val p = files.find(_.toUri.toString.endsWith("csv")).get
    logger.info(s"Loading $p")
    val src = Source.fromFile(p.toUri)
    var res = List.empty[(Long, String)]
    try {
      var i = 0
      for (line ← src.getLines) {
        i += 1
        if (line.length > 0) {
          try {
            val s = line.split(',')
            val ts = s(0).toLong
            val obj = s(1)
            res = (ts -> obj) :: res
          } catch {
            case _: RuntimeException ⇒
              logger.info(s"Skip malformed [$line]")
          }
        }
      }
    } finally {
      src.close()
    }
    logger.info(s"Loaded ${res.size} from file")
    res
  }

  init()

}