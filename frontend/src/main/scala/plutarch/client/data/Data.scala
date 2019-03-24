package plutarch.client.data

import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.metrics.Conf

trait Data {
  def get(metricName: String): MetricData
  def getControl: DataControl
  def getDataSource: DataSource
}

trait MetricData {
  def getMetricConf: Conf
  def get(aggregation: Aggregation): AggregationData
  def getObjectById(id: Int): DataObjectOps
  def incarnation: Long
  // experimental
  def getCombined(objId: Int, aggregations: Seq[Aggregation]): CombinedAggregationData // this uses aggregations as objects!
}

trait AggregationData {
  def getExact(targetScale: Int, x: Double, y: Double): DataView
  def get(targetScale: Int, x: Double, y: Double): DataView
}