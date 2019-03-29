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

import plutarch.client.data.CombinedDataView.SimpleMap
import plutarch.shared.data.Aggregations.Aggregation

class CombinedAggregationData(objId: Int, aggregations: Seq[Aggregation], aggregationDatas: Map[Aggregation, AggregationData]) {
  def get(targetScale: Int, x: Double, y: Double): CombinedDataView = {
    val views = for ((agg, idx) ← aggregations.zipWithIndex) yield {
      val aggregationData = aggregationDatas(agg)
      (idx + 1) -> aggregationData.getExact(targetScale, x, y)
    }
    val mviews = views.toMap
    val first = views.head._2
    new CombinedDataView {
      def reqLeft: Double = first.reqLeft
      def reqRight: Double = first.reqRight
      def left: Double = first.left
      def right: Double = first.right
      def step: Double = first.step
      def bestScale: Int = first.bestScale
      val values: SimpleMap[Double, collection.Map[Int, Double]] = {
        new SimpleMap[Double, collection.Map[Int, Double]] {
          def get(key: Double): collection.Map[Int, Double] = mviews.mapValues(_.values.get(key).get.apply(objId))
          def contains(key: Double): Boolean = first.values.has(key)
        }
      }
      def info: DataView.Info = first.info // todo this is used for subscripion of online! but we have to add aggregation list here too!
    }
  }
}