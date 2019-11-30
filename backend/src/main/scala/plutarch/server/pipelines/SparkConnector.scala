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
import org.apache.spark.sql.types._
import org.apache.spark.sql.{ DataFrame, Dataset, Row }

import scala.concurrent.duration._
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.data.store
import plutarch.server.data.store.{ DefaultMetricStoreCreatorCreator, MetricStoreCreator }
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations.Sum
import plutarch.shared.data.metrics.Conf

import scala.concurrent.{ Await, Future }

object SparkConnector {

  private val BATCH_SIZE = 100
  private val TIMEOUT = 10 seconds

  var system: ActorSystem = _
  var metricManager: MetricManager = _
  var webSocketFlowCoordinator: WebSocketFlowCoordinator = _

  def init(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem): Unit = {
    this.metricManager = metricManager
    this.system = system
    this.webSocketFlowCoordinator = webSocketFlowCoordinator
  }

  private def getConf(name: String): Conf = Conf(
    name = name,
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Sum),
    withTotal = true)

  def create(name: String): SparkConnector = {
    create(name, MetricStoreCreator.newConf(name))
  }

  def create(name: String, headerBaseSize: Int, storeBaseSize: Int): SparkConnector = {
    val storeConf = new store.MetricStoreCreator.Conf(name, Map(
      DefaultMetricStoreCreatorCreator.HEADER_BASE_SIZE -> headerBaseSize,
      DefaultMetricStoreCreatorCreator.STORE_BASE_SIZE -> storeBaseSize))
    create(name, storeConf)
  }

  def create(name: String, storeConf: MetricStoreCreator.Conf): SparkConnector = new SparkConnector() {
    private val system = SparkConnector.system

    import system.dispatcher

    override val conf: Conf = getConf(name)
    override val metric: Metric = metricManager.getOrCreate(conf, storeConf)

    override def publish(t: Long, data: Seq[(String, Double)]): Future[Unit] = {
      metric.add(t, data)
    }

    def loadDataFrame(df: DataFrame): Unit = {
      import org.apache.spark.sql.functions.col
      if (df.schema.fields.length == 3) {
        // check if schema is (Timestamp, String, Double)
        val ts = df.schema.fields(0)
        val objectName = df.schema.fields(1)
        val value = df.schema.fields(2)
        if (ts.dataType.isInstanceOf[LongType] &&
          objectName.dataType.isInstanceOf[StringType] &&
          value.dataType.isInstanceOf[NumericType]) {
          df.sort(col(ts.name).asc).collect().iterator.grouped(BATCH_SIZE).foreach { group: Seq[Row] ⇒
            val futures = group.map { row ⇒
              val t = row.getLong(0)
              val objectName = row.getString(1)
              val value = row.getAs[Number](2).doubleValue()
              publish(t, Seq((objectName, value)))
            }
            Await.result(Future.sequence(futures), TIMEOUT)
          }
          metric.freeze()
          return
        }
      } else if (df.schema.length == 2) {
        // check if schema is (Timestamp, Seq(String, Double))
        val ts = df.schema.fields(0)
        val seq = df.schema.fields(1)
        if (ts.dataType.isInstanceOf[LongType] &&
          seq.dataType.isInstanceOf[ArrayType] &&
          seq.dataType.asInstanceOf[ArrayType].elementType.isInstanceOf[StructType]) {
          val rec = seq.dataType.asInstanceOf[ArrayType].elementType.asInstanceOf[StructType]
          if (rec.fields.length == 2) {
            val objectName = rec.fields(0)
            val value = rec.fields(1)
            if (objectName.dataType.isInstanceOf[StringType] &&
              value.dataType.isInstanceOf[NumericType]) {
              df.sort(col(ts.name).asc).collect().iterator.grouped(BATCH_SIZE).foreach { group: Seq[Row] ⇒
                val futures = group.map { row ⇒
                  val t = row.getLong(0)
                  val values = row.getSeq[Row](1).map(r ⇒ (r.getString(0), r.getAs[Number](1).doubleValue()))
                  publish(t, values)
                }
                Await.result(Future.sequence(futures), TIMEOUT)
              }
              metric.freeze()
              return
            }
          }
        }
      }
      throw new RuntimeException("Can't recognize DataFrame format")
    }

    def loadDataSet[T <: TSGeneric](ds: Dataset[T]): Unit = {
      import org.apache.spark.sql.functions.col
      ds.sort(col("t").asc).collect().iterator.grouped(BATCH_SIZE).foreach { group: Seq[T] ⇒
        val futures = group.map {
          case TSObjectRecord(t, objectName, value) ⇒
            publish(t, Seq((objectName, value)))
          case TSObjectsRecord(t, objectsValues) ⇒
            publish(t, objectsValues.map(_.toTuple))
          case TSObjectsTupleRecord(t, objectsValues) ⇒
            publish(t, objectsValues)
          case _ ⇒
            Future.successful()
        }
        Await.result(Future.sequence(futures), TIMEOUT)
      }
      metric.freeze()
    }
  }

  def drop(name: String): Unit = {
    metricManager.drop(name)
  }

  // Spark DS parameter types

  sealed trait TSGeneric {
    def t: Long
  }

  case class TSObjectRecord(t: Long, objectName: String, value: Double) extends TSGeneric

  case class ObjectValue(objectName: String, value: Double) {
    def toTuple: (String, Double) = (objectName, value)
  }

  case class TSObjectsRecord(t: Long, objectsValues: Seq[ObjectValue]) extends TSGeneric

  case class TSObjectsTupleRecord(t: Long, objectsValues: Seq[(String, Double)]) extends TSGeneric

}

trait SparkConnector {
  def conf: Conf
  def metric: Metric
  def publish(t: Long, data: Seq[(String, Double)]): Future[Unit]
  def loadDataSet[T <: SparkConnector.TSGeneric](ds: Dataset[T]): Unit
  def loadDataFrame(df: DataFrame): Unit
}