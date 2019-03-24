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