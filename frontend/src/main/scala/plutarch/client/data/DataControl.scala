package plutarch.client.data

import plutarch.client.graph.Geometry
import plutarch.shared.data.Aggregations.Aggregation

object DataControl {
  def apply(): DataControl = new DataControl()
}

class DataControl {
  private var data = Map.empty[(String, Aggregation), Set[Geometry.Context]]
  def getContexts(metricName: String, aggregation: Aggregation): Set[Geometry.Context] = {
    data.getOrElse((metricName, aggregation), Set.empty)
  }
  def set(metricName: String, aggregation: Aggregation, ctx: Geometry.Context): Unit = {
    val curSet = data.getOrElse((metricName, aggregation), Set.empty)
    data += ((metricName, aggregation) -> (curSet + ctx))
  }
  def unset(metricName: String, aggregation: Aggregation, ctx: Geometry.Context): Unit = {
    val curSet = data.getOrElse((metricName, aggregation), Set.empty)
    data += ((metricName, aggregation) -> (curSet - ctx))
  }
}