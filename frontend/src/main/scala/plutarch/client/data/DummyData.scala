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

package plutarch.client.data

import plutarch.client.experemental.{ JSMap, Values }
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.{ Aggregations, DataObject }
import plutarch.shared.data.metrics.Conf
import Aggregations._

import scala.collection.Map
import scala.collection.immutable.TreeSet

object DummyData {

  object DummyDataViewInfo extends DataView.Info

  private val dummyConf = Conf(
    name = "Dummy",
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Sum, Max, Count, PercentileCont(0.1), PercentileCont(0.5), PercentileCont(0.9)),
    withTotal = false)

  def getMetricDummy(name: String): Conf = dummyConf

  def createDummy(koef1: Double, koef2: Double, koef3: Double): Data = new Data {
    val dataControl: DataControl = DataControl()

    def getDataSource: DataSource = ???
    def getControl: DataControl = dataControl

    private var scaleTree = TreeSet.empty[Int](Ordering.Int.reverse)
    def get(metricName: String): MetricData = new MetricData {
      def incarnation: Long = 0L

      def getMetricConf: Conf = dummyConf

      private val objs = Map(
        1 -> DataObjectOps(DataObject(1, "Dummy1", "#FF0000")),
        2 -> DataObjectOps(DataObject(2, "Dummy2", "#00FF00")),
        3 -> DataObjectOps(DataObject(3, "Dummy3", "#0000FF")),
        4 -> DataObjectOps(DataObject(4, "Dummy4", "#00FFFF")))

      def getObjectById(id: Int): DataObjectOps = objs(id)

      def get(aggregation: Aggregation): AggregationData = new AggregationData {
        def getExact(targetScale: Int, x: Double, y: Double): DataView = ???
        def get(targetScale: Int, x: Double, y: Double): DataView = new DataView {

          def info: DataView.Info = DummyDataViewInfo

          def reqLeft: Double = x

          def reqRight: Double = y

          private val metric = dummyConf

          if (scaleTree.isEmpty) {
            scaleTree ++= metric.scales
          }

          private val scale: Int = scaleTree.from(targetScale).headOption.getOrElse(1)

          private val step0 = metric.step * scale
          private val x0 = (x.toLong / step0) * step0
          private val x1 = if (x0 > x) x0 - step0 else x0
          private val y0 = (y.toLong / step0) * step0
          private val y1 = if (y0 < y) y0 + step0 else y0

          def left: Double = x1

          def right: Double = y1

          def step: Double = step0

          def bestScale: Int = scale

          val values: Values[Map[Int, Double]] = {
            val data = JSMap.empty[Double, Map[Int, Double]]
            var key = left
            while (key <= right) {
              val value1 = koef3 + koef2 * Math.sin(koef1 * key)
              val value2 = koef3 * 2 + koef2 * 2 * Math.sin(koef1 * 2 * key)
              val value3 = koef3 * 4 + koef2 * 4 * Math.sin(koef1 * 4 * key)
              val value4 = koef3 * 8 + koef2 * 8 * Math.sin(koef1 * 256 * key)
              //          if (value4 < 0) {
              //            data.update(key, Map(1 -> value1, 2 -> value2, 3 -> value3))
              //          } else if (value3 > 0) {
              //            data.update(key, Map(1 -> value1, 2 -> value2, 3 -> value3, 4 -> value4))
              //          }
              if (value4 < 0) {
                data.set(key, Map(1 -> value1.abs, 2 -> value2.abs, 3 -> value3.abs))
              } else if (value3 > 0) {
                data.set(key, Map(1 -> value1.abs, 2 -> value2.abs, 3 -> value3.abs, 4 -> value4.abs))
              }
              //data.update(key, Map(1 -> value1, 2 -> value2, 3 -> value3, 4 -> value4))
              key += step
            }
            JSMap.asValues(data)
          }
        }
      }
      def getCombined(objId: Int, aggregations: Seq[Aggregation]): CombinedAggregationData = ???
    }
  }
}
