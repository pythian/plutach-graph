package plutarch.client.data

import scala.collection.Map
import scala.collection.mutable.{ Map ⇒ MMap }
import plutarch.client.experemental.JSMap
import plutarch.shared.collection.DisjointIntervalSetDouble
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers
import plutarch.shared.data.metrics.Meta
import CacheScale._
import plutarch.shared.Protocol
import plutarch.shared.Protocol.Interval
import plutarch.shared.data.Picklers.{ CombinedDoubleData, CombinedIntData, CombinedLongData }
import slogging.LazyLogging

// todo: reduce unnecessary object messaging every currect update: keep is cached what was sent for the session? or API to get only "new" objects instead of overlaping?
object CacheScale {
  case class MInterval(var left: Double, var right: Double, var rawRight: Double) {
    def collapse(value: Double): Unit = {
      left = value
      right = value
    }
    def adjLeft(value: Double): Unit = {
      if (value > left) left = value
    }
    def contains(that: Interval): Boolean = {
      this.left <= that.left && that.right <= this.right
    }
    def contains(thatLeft: Double, thatRight: Double): Boolean = {
      this.left <= thatLeft && thatRight <= this.right
    }
  }

  def create(scale: Int, cacheState: CacheState, meta: Meta, aggregation: Aggregation, dataSource: DataSource): CacheScale =
    new CacheScale(scale, cacheState, meta, aggregation, dataSource)

  case class CacheScaleInfo(state: CacheScaleState) extends DataView.Info {
    def needSubscribe(left: Double, right: Double): Boolean = {
      val adjInterval = Interval(left - state.checkDist, right + state.checkDist)
      adjInterval.isIntersected(Interval(state.watermark, state.current))
    }
  }

  case class CacheScaleDataView(
      info:      CacheScaleInfo,
      reqLeft:   Double,
      reqRight:  Double,
      left:      Double,
      right:     Double,
      step:      Double,
      bestScale: Int,
      values:    JSMap[Double, Map[Int, Double]]) extends DataView

  case class WatermarkRequest(req: Protocol.WSHistRequest, current: Double)

  class CacheScaleState(step: Long, meta: Meta) {
    var checkDist: Double = 150.0 * step
    var reqDist: Double = 2 * checkDist
    var maxCached: Double = 100000.0 * step
    var maxRequested: Int = 20
    var threshold: Double = 200 * step
    var current: Double = step * (meta.current / step)
    var watermark: Double = current - step
    var watermarkRequest: Option[WatermarkRequest] = None
    var lastInterval: Option[MInterval] = None
  }

}

class CacheScale(scale: Int, cacheState: CacheState, meta: Meta, aggregation: Aggregation, dataSource: DataSource) extends LazyLogging {
  cache ⇒

  private val step = meta.conf.step * scale
  private val data = JSMap.empty[Double, Map[Int, Double]]
  private val cachedSet = DisjointIntervalSetDouble.empty
  private val requestedSet = DisjointIntervalSetDouble.empty
  private val state = new CacheScaleState(step, meta)

  //  private var logid = 0L
  //  private val debug_enabled = true
  //  @inline
  //  private def debug(text: String): Unit = if (debug_enabled) {
  //    if (scale == 300 && aggregation == Sum) {
  //      println(s"[$logid] $text")
  //      logid += 1
  //    }
  //  }

  def info: CacheScaleInfo = CacheScaleInfo(state)

  // (past, recent)
  private def toIntervals(x: Double, y: Double): (Option[Interval], Option[Interval]) = {
    var _x = (x.toLong / step) * step
    if (_x > x) _x -= step
    var _y = (y.toLong / step) * step
    if (_y < y) _y += step
    _y += step // add step as we need right end included
    if (_y <= state.watermark) {
      (Some(Interval(_x, _y)), None)
    } else if (_x >= state.watermark) {
      (None, Some(Interval(_x, _y)))
    } else {
      (Some(Interval(_x, state.watermark)), Some(Interval(state.watermark, _y)))
    }
  }

  def get(x: Double, y: Double, bestScale: Int, isRequested: Boolean): Option[DataView] = {
    val (optPast, optRecent) = toIntervals(x, y)
    (optPast, optRecent) match {
      case (Some(past), Some(recent)) ⇒
        //debug(s"agg=$aggregation, scale=$scale, (0): get($x, $y), state.lastInterval=${state.lastInterval}, recent=$recent, contains=${state.lastInterval.exists(_.contains(recent.left, recent.right.min(state.current)))}")
        if (state.lastInterval.exists(_.contains(recent.left, recent.right.min(state.current)))) {
          if (isRequested) request(past.left, past.right)
          val diff = cachedSet.diff(past)
          if (diff.isEmpty) {
            //debug(s"agg=$aggregation, scale=$scale, (1): get($x, $y), diff.isEmpty=true")
            Some(CacheScaleDataView(info, x, y, past.left, recent.right.min(state.current + step), step, bestScale, data))
          } else {
            //debug(s"agg=$aggregation, scale=$scale, (2): get($x, $y), diff.isEmpty=false)")
            None
          }
        } else {
          //debug(s"agg=$aggregation, scale=$scale, (3): get($x, $y), contains=false")
          state.watermark = state.current
          state.lastInterval.foreach(_.adjLeft(state.watermark))
          if (isRequested) request(past.left, state.watermark)
          val diff = cachedSet.diff(past.left, state.watermark)
          if (diff.isEmpty || diff.map(_.length).sum <= 3 * step) { // tolerance prevents blinking
            //debug(s"agg=$aggregation, scale=$scale, (4): get($x, $y), diff.isEmpty=ture")
            Some(CacheScaleDataView(info, x, y, past.left, state.watermark + step, step, bestScale, data))
          } else {
            //debug(s"agg=$aggregation, scale=$scale, (4): get($x, $y), diff.isEmpty=false")
            None
          }
        }
      case (Some(past), None) ⇒
        if (isRequested) request(past.left, past.right)
        val diff = cachedSet.diff(past)
        if (diff.isEmpty) {
          Some(CacheScaleDataView(info, x, y, past.left, past.right, step, bestScale, data))
        } else {
          None
        }
      case (None, Some(recent)) ⇒
        if (state.lastInterval.exists(_.contains(recent.left, recent.right.min(state.current)))) {
          Some(CacheScaleDataView(info, x, y, recent.left, recent.right.min(state.current + step), step, bestScale, data))
        } else {
          state.watermark = state.current
          state.lastInterval.foreach(_.adjLeft(state.watermark))
          if (isRequested) request(recent.left, state.watermark)
          val diff = cachedSet.diff(recent.left, state.watermark)
          if (diff.isEmpty || diff.map(_.length).sum <= 3 * step) { // tolerance prevents blinking
            Some(CacheScaleDataView(info, x, y, recent.left, state.watermark + step, step, bestScale, data))
          } else {
            None
          }
        }
      case (_, _) ⇒
        None
    }
  }

  // do we really need this ???
  def getAny(x: Double, y: Double, bestScale: Int, pct: Double): Option[DataView] = {
    val (optPast, optRecent) = toIntervals(x, y)
    optPast match {
      case Some(Interval(left, right)) ⇒
        val intersect = cachedSet.intersect(left, right + step)
        val intersectLength = intersect.map(_.length).sum
        val minimalLength = pct * (y - x)
        if (intersectLength > minimalLength) {
          Some(CacheScaleDataView(info, x, y, left, optRecent.map(_.right).getOrElse(right), step, bestScale, data))
        } else {
          None
        }
      case None ⇒
        None
    }
  }

  private def request(requesting: Seq[Interval]): Protocol.WSHistRequest = {
    requestedSet.add(requesting)
    val req = dataSource.request(meta.conf.name, aggregation, scale, requesting)
    cacheState.requests += (req.requestId -> req)
    req
  }

  private def request(left: Double, right: Double): Unit = {
    val adjInterval = Interval(left - state.checkDist, (right + state.checkDist).min(state.watermark))
    // not cached and not yet requested
    val remaining = cachedSet.diff(adjInterval).flatMap(interval ⇒ requestedSet.diff(interval))
    if (remaining.nonEmpty) {
      if (cacheState.requests.size < state.maxRequested) {
        val adjInterval = Interval(left - state.reqDist, (right + state.reqDist).min(state.watermark))
        val requesting = cachedSet.diff(adjInterval).flatMap(interval ⇒ requestedSet.diff(interval))
        request(requesting)
      } else {
        logger.warn(s"Skip requesting data because of requests queue size=${cacheState.requests.size} >= maxRequested=${state.maxRequested}")
      }
    }
  }

  def receive(req: Protocol.WSHistRequest, data: Picklers.CombinedData[_]): Unit = {
    // this copy/paste is ugly but reliable performance (kind of predicted specialization)

    //var minKey = Double.MaxValue
    //var maxKey = Double.MinValue

    data match {
      case d: CombinedDoubleData ⇒
        for ((key, objs) ← d.data) {
          val map = MMap.empty[Int, Double]
          for ((objId, value) ← objs) {
            map.update(objId, value)
          }
          cache.data.set(key, map)

          //minKey = minKey.min(key)
          //maxKey = maxKey.max(key)
        }
      case d: CombinedIntData ⇒
        for ((key, objs) ← d.data) {
          val map = MMap.empty[Int, Double]
          for ((objId, value) ← objs) {
            map.update(objId, value)
          }
          cache.data.set(key, map)

          //minKey = minKey.min(key)
          //maxKey = maxKey.max(key)
        }
      case d: CombinedLongData ⇒
        for ((key, objs) ← d.data) {
          val map = MMap.empty[Int, Double]
          for ((objId, value) ← objs) {
            map.update(objId, value)
          }
          cache.data.set(key, map)

          //minKey = minKey.min(key)
          //maxKey = maxKey.max(key)
        }
    }

    //debug(s"recieved $req, min=$minKey, max=$maxKey")

    requestedSet.remove(req.intervals)
    cachedSet.add(req.intervals)
    state.watermarkRequest match {
      case Some(WatermarkRequest(_, newWatermark)) ⇒
        state.watermark = newWatermark
        state.lastInterval.foreach(_.adjLeft(state.watermark))
        state.watermarkRequest = None

      //debug(s"agg=$aggregation, scale=$scale, (1): receive(${req.intervals}), state.lastInterval=${state.lastInterval}")
      case _ ⇒
    }
    for (interval ← cachedSet.shrink(state.maxCached)) {
      var key = interval.left
      while (key < interval.right) {
        cache.data.delete(key)
        key += step
      }
    }
    //logger.info(s"metric=${meta.conf.name}, agg=${aggregation.name}, step=$step, cachedSet.rawIntervalsCount=${cachedSet.rawIntervalsCount}, cachedSet.mergedIntervalsCount=${cachedSet.mergedIntervalsCount}")
  }

  /*
  * Data from state.watermark TO state.current may or may not exists and in general case must be queried
  * */
  def receive(rawKey: Long, data: Protocol.CurrentData): Unit = {
    //debug(s"agg=$aggregation, scale=$scale, (0): receive(${data.key}), state.lastInterval=${state.lastInterval}")

    state.lastInterval match {
      case Some(last) ⇒
        if (last.right != data.key && last.right + step != data.key) {
          last.left = data.key
        }
        last.right = data.key
        last.rawRight = rawKey
      case None ⇒
        state.lastInterval = Some(MInterval(data.key, data.key, rawKey))
    }

    //debug(s"agg=$aggregation, scale=$scale, (1): receive(${data.key}), state.lastInterval=${state.lastInterval}")
    cache.data.set(data.key, data.data)
  }

  def updateCurrent(current: Long): Unit = {
    ////debug(s"agg=$aggregation, scale=$scale, (0): updateCurrent($current), state.current=${state.current}, state.lastInterval=${state.lastInterval}")

    state.current = (current / step) * step

    if (state.lastInterval.exists(last ⇒ last.rawRight != current)) {
      state.lastInterval = None
    }

    if (state.watermarkRequest.isEmpty && state.watermark < state.current - state.threshold) {
      val graphContexts = cacheState.control.getContexts(meta.conf.name, aggregation)
      val interval = Interval(state.watermark - state.checkDist, state.current + state.checkDist)
      if (graphContexts.exists(p ⇒ p.getState.step == step && Interval(p.getState.gmin.x, p.getState.gmax.x).isIntersected(interval))) {
        val req = request(Seq(Interval(state.watermark, state.current)))
        state.watermarkRequest = Some(WatermarkRequest(req, state.current))
      } else {
        // note: we have to keep just to prevent requesting data to fast to avoid race condition (to be fixed with chaining futures of saving data and sending info)
        state.watermark = state.current
        state.lastInterval.foreach(_.adjLeft(state.watermark))
      }
    }

    ////debug(s"agg=$aggregation, scale=$scale, (1): updateCurrent($current), state.current=${state.current}, state.lastInterval=${state.lastInterval}")
  }

  def getAll: Seq[Interval] = cachedSet.getAll
  def getRequested: Seq[Interval] = requestedSet.getAll
  def reset(): Unit = {
    state.watermarkRequest = None
    state.lastInterval = None
    requestedSet.clear()
  }
}