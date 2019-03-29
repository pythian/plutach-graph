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
import plutarch.shared.Protocol
import plutarch.shared.data.Picklers
import plutarch.shared.data.metrics.{ Conf, Meta }

object CacheData {
  val checkDist = 150
  val loadDist = 300
  def create(dataSource: DataSource): CacheData = new CacheData(dataSource)
}

class CacheState(val control: DataControl) {
  var requests = Map.empty[Int, Protocol.WSHistRequest]
  var metrics = Map.empty[String, CacheMetricData]
  var metricConfs = Map.empty[String, Conf]
  var onReceive = Option.empty[Protocol.WSHistRequest ⇒ Unit]
  var onCurrentsReceive = Option.empty[Protocol.WSCurrents ⇒ Unit]
}

class CacheData(dataSource: DataSource) extends Data {
  val state = new CacheState(DataControl())

  def getControl: DataControl = state.control
  def getDataSource: DataSource = dataSource

  def isRegistered(meta: Meta): Boolean = {
    state.metricConfs.contains(meta.conf.name)
  }

  def registerMetric(meta: Meta): Unit = {
    state.metricConfs += (meta.conf.name -> meta.conf)
    state.metrics += (meta.conf.name -> CacheMetricData.create(state, meta, dataSource))
  }

  def unregisterMetric(metricName: String): Unit = {
    state.metricConfs -= metricName
    state.metrics -= metricName
  }

  def get(metricName: String): MetricData = state.metrics.get(metricName) match {
    case Some(metricData) ⇒ metricData
    case None ⇒
      throw new Exception(s"Requested not registered metric=$metricName")
  }

  def receive(data: Picklers.CombinedData[_]): Unit = {
    val req = state.requests(data.requestId)
    state.requests -= data.requestId
    state.metrics.get(req.metric) match {
      case Some(metricData) ⇒ metricData.receive(req, data)
      case None ⇒
        throw new Exception(s"Recieved data for not registered metric=$req.metric")
    }
    state.onReceive.foreach(handler ⇒ handler(req))
  }

  def receive(currents: Protocol.WSCurrents): Unit =
    state.metrics.get(currents.metric) match {
      case Some(metricData) ⇒
        metricData.receive(currents)
        state.onCurrentsReceive.foreach(handler ⇒ handler(currents))
      case None ⇒
    }

  def setOnReceive(handler: Protocol.WSHistRequest ⇒ Unit): Unit = {
    state.onReceive = Some(handler)
  }

  def setOnCurrentsReceive(handler: Protocol.WSCurrents ⇒ Unit): Unit = {
    state.onCurrentsReceive = Some(handler)
  }

  def reset(): Unit = {
    state.requests = Map.empty
    state.metrics.values.foreach(m ⇒ m.reset())
  }

}

/*

object CacheData {
  var respNo = 0
  var id = 0
  def apply(dom: Domain, send: String ⇒ Boolean, optMetricId: Option[Int] = None)(implicit ctx: Ctx.Owner) = new CacheData(dom, send, optMetricId)
  val rxRequestQueue = Var(Set.empty[(Int, Long)])
  val rxCheckDist = Var(150)
  val rxLoadDist = Var(300)
}

class CacheData(dom: Domain, send: String ⇒ Boolean, optMetricId: Option[Int] = None)(implicit ctx: Ctx.Owner) {
  import CacheData._
  val maxLength1 = 20000
  val minAnyPCT = 0.8
  def maxLength(scale: Int): Int = (maxLength1 / Math.log10(scale + 1)).toInt
  private val scales = immutable.TreeSet[Int]() ++ (dom.scales :+ 1)
  private val caches = scales.map(scale ⇒ (scale, new StepCache(dom, scale, scale * dom.step, maxLength(scale), send, optMetricId))).toMap
  val rxNotify: Rx[Option[(Long, Long, Long)]] = Rx {
    caches
      .values
      .toList
      .flatMap(cache ⇒ cache.rxNotifyRest().map(n ⇒ (n._1, (cache.step, n._2, n._3))))
      .optionMaxBy(_._1)
      .map(_._2)
  }
  def isRequested(scale: Int, x: Long, y: Long, loadServer: Boolean): Option[TheCacheDataView] = {
    caches(scale).isRequested(x, y, loadServer)
  }
  def parseResponse(input: js.Dynamic): Unit = {
    val id = input.id.asInstanceOf[Int]
    val scale = input.scale.asInstanceOf[Int]
    val data = input.data
    caches(scale).parseResponse(id, data)
  }
  def requestAny(scale: Int, x: Long, y: Long): Option[TheCacheDataView] = {
    caches(scale).requestAny(x, y, minAnyPCT)
  }
  def clear(): Unit = {
    rxRequestQueue() = Set.empty
    caches.foreach(_._2.clear())
  }
}

class StepCacheDictVersion(dom: Domain, val scale: Int, val step: Long, maxLength: Long, send: String ⇒ Boolean, optMetricId: Option[Int] = None)(implicit ctx: Ctx.Owner) {
  cache ⇒
  import CacheData._
  // metricId->Key->objectId->DataValue
  private val metrics = emptyMetricsDict()
  private val cachedSet = new IntervalSet
  private val requestedSet = new IntervalSet
  private val requests = mutable.Map[Int, (List[Interval], Long, Long)]()
  val rxNotifyRest: Var[Option[(Int, Long, Long)]] = Var(None)

  def keyRoundToStep(k: Long): Long = (k / step) * step

  abstract class TheStepCacheDataView(val left: Long, val right: Long, val minKey: Long, val maxKey: Long) extends TheCacheDataView {
    view ⇒
    def metrics: MetricsRO = cache.metrics.asIntIndexedRO.mapValues(
      keys ⇒ keys.asLongIndexedRO(view.keys).mapValues(
        obj ⇒ obj.asIntIndexedRO.mapValues(
          values ⇒ values.toRO
        )
      )
    )
    def keyToStep(key: Long): Long = step
    val steps: List[Long] = List(step)
    def getNeighbours(x: Double): (Option[Long], Option[Long]) =
      if (minKey <= x && x <= maxKey) {
        val leftNeighbour = keyRoundToStep(x.toLong)
        (Some(leftNeighbour), Some(leftNeighbour + step))
      } else
        (None, None)
  }

  def requestAny(x: Long, y: Long, minAnyPCT: Double): Option[TheCacheDataView] = {
    val minKeyCalculated = keyRoundToStep(x)
    val maxKeyCalculated = keyRoundToStep(y) + step
    val interval = Interval(minKeyCalculated, maxKeyCalculated + step)
    val intersect = cachedSet.intersect(interval)
    if (intersect.map { i ⇒ i.right - i.left }.sum > minAnyPCT * (y - x) / step)
      Some(new TheStepCacheDataView(x, y, minKeyCalculated, maxKeyCalculated) {
        def keys: KeysRO = intersect.sortBy(_.left).flatMap(i ⇒ i.left.until(i.right, step))
      })
    else None
  }



  def parseResponse(id: Int, inputData: js.Dynamic): Unit = if (rxRequestQueue.now.contains((id, step))) {
    rxRequestQueue() = rxRequestQueue.now - ((id, step))
    optMetricId match {
      case None           ⇒ parseResponse2(id, inputData)
      case Some(metricId) ⇒ parseResponse3(id, inputData, metricId)
    }
    val (intervals, x, y) = requests(id)
    cachedSet.add(intervals)
    respNo += 1
    rxNotifyRest() = Some((respNo, x, y))
    requests -= id
    shrink()
  }

  def parseResponse2(id: Int, inputData: js.Dynamic): Unit = {
    //List[(Long, List[(Int, List[(Int, DataValue)])])]
    val keyData = inputData.asInstanceOf[js.Array[js.Array[js.Dynamic]]]
    keyData.foreach { keyJs ⇒
      val key = js.Dynamic.global.Number(keyJs(0)).asInstanceOf[Double].toLong
      val objsData = keyJs(1).asInstanceOf[js.Array[js.Array[js.Dynamic]]]
      objsData.foreach { objJs ⇒
        val objId = objJs(0).asInstanceOf[Int]
        val metricsData = objJs(1).asInstanceOf[js.Array[js.Array[js.Dynamic]]]
        metricsData.foreach { metricJs ⇒
          val metricId = metricJs(0).asInstanceOf[Int]
          val dataValue = Protocol.DataValue(
            metricJs(1).cnt.asInstanceOf[Int],
            metricJs(1).sum.asInstanceOf[Double],
            metricJs(1).max.asInstanceOf[Double],
            metricJs(1).min.asInstanceOf[Double]
          )
          val keysData = this.metrics.getOrElseUpdate(metricId.toString, emptyKeysDataDict())
          val objsData = keysData.getOrElseUpdate(key.toString, emptyObjectsDataDict())
          val objDataValue = objsData.getOrElseUpdate(objId.toString, new DataValue(scale))
          objDataValue.add(dataValue)
        }
      }
    }
  }

  def parseResponse3(id: Int, inputData: js.Dynamic, metricId: Int): Unit = {
    //List[(Long, List[(Int, DataValue)])]
    val keyData = inputData.asInstanceOf[js.Array[js.Array[js.Dynamic]]]
    keyData.foreach { keyJs ⇒
      val key = js.Dynamic.global.Number(keyJs(0)).asInstanceOf[Double].toLong
      val objsData = keyJs(1).asInstanceOf[js.Array[js.Array[js.Dynamic]]]
      objsData.foreach { objJs ⇒
        val objId = objJs(0).asInstanceOf[Int]
        val dataValue = Protocol.DataValue(
          objJs(1).cnt.asInstanceOf[Int],
          objJs(1).sum.asInstanceOf[Double],
          objJs(1).max.asInstanceOf[Double],
          objJs(1).min.asInstanceOf[Double]
        )
        val keysData = this.metrics.getOrElseUpdate(metricId.toString, emptyKeysDataDict())
        val objsData = keysData.getOrElseUpdate(key.toString, emptyObjectsDataDict())
        val objDataValue = objsData.getOrElseUpdate(objId.toString, new DataValue(scale))
        objDataValue.add(dataValue)
      }
    }
  }

  def shrink(): Unit = {
    for {
      interval ← cachedSet.shrink(maxLength * step)
      _ = requestedSet.remove(interval)
      key ← interval.left.until(interval.right, step)
      metric ← metrics.keys
    } metrics(metric.toString).remove(key.toString)
  }

  def clear(): Unit = {
    metrics.clear()
    cachedSet.clear()
    requestedSet.clear()
    requests.clear()
    rxNotifyRest() = None
  }
}*/
