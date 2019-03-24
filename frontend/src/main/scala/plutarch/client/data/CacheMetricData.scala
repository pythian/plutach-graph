package plutarch.client.data

import plutarch.client.experemental.JSMap
import plutarch.shared.Protocol
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers
import plutarch.shared.data.metrics.{ Meta, Conf }

object CacheMetricData {
  def create(cacheState: CacheState, meta: Meta, dataSource: DataSource): CacheMetricData =
    new CacheMetricData(cacheState, meta, dataSource)
}

class CacheMetricData(cacheState: CacheState, meta: Meta, dataSource: DataSource) extends MetricData {
  var incarnation: Long = 0L

  private val aggregationDatas = Map.empty[Aggregation, CacheAggregationData] ++
    meta.conf.aggregations.map(agg ⇒ agg -> CacheAggregationData.create(cacheState, meta, agg, dataSource))
  private val dataObjects = JSMap.empty[Int, DataObjectOps]
  def getMetricConf: Conf = meta.conf
  def getObjectById(id: Int): DataObjectOps = dataObjects.get(id).get
  def get(aggregation: Aggregation): AggregationData = aggregationDatas(aggregation)
  def receive(req: Protocol.WSHistRequest, data: Picklers.CombinedData[_]): Unit = {
    data.objects.foreach { obj ⇒
      dataObjects.set(obj.id, DataObjectOps(obj))
    }
    aggregationDatas(data.aggregation).receive(req, data)
  }
  def receive(currents: Protocol.WSCurrents): Unit = {
    for (data ← currents.data) {
      for {
        obj ← data.objects
        if !dataObjects.has(obj.id)
      } {
        dataObjects.set(obj.id, DataObjectOps(obj))
      }
      aggregationDatas(data.aggregation).receive(currents.key, data)
    }
    for (aggregationData ← aggregationDatas.values) {
      aggregationData.updateCurrent(currents.key)
    }
    incarnation += 1
  }

  def reset(): Unit = {
    aggregationDatas.values.foreach(a ⇒ a.reset())
  }

  def getCombined(objId: Int, aggregations: Seq[Aggregation]): CombinedAggregationData = {
    new CombinedAggregationData(objId, aggregations, aggregationDatas)
  }
}