package plutarch.client.graph

import plutarch.client.data._
import plutarch.client.data.DataView._
import slogging.LazyLogging
import Geometry._
import plutarch.client.data.CacheScale.MInterval
import plutarch.client.experemental.{ DateTools, JSArray }
import plutarch.shared.colors.Colors
import plutarch.shared.data.{ Aggregations, Charts }
import plutarch.client.experemental.Tools.timing
import plutarch.shared.Protocol
import plutarch.shared.Protocol.Interval
import plutarch.shared.data.Aggregations.Aggregation
import scala.collection.mutable.ListBuffer

/*
* Graph component talks to Data object to get data required to draw it with geometry context
* */

// todo: special case of percentile river-graph
// todo: select interval + integral
// todo: position
// todo: show only total

object Graph {
  private var currId: Int = 0
  def apply(graphControlState: GraphControlState)(ctx: Geometry.Context): Graph = {
    currId += 1
    new Graph(graphControlState, currId)(ctx)
  }
}

class GraphState {
  var drawTicks = true
  var alpha = 0.7
  var minPTP = 20
  var dataView: Option[DataView] = None
  var bestScale: Int = 0
  var drawScale: Int = 0
  var drawAdjCurrent: Double = 0.0
  var drawDataIncarnation: Long = -1L
  var subscribedForRT: Option[(String, Aggregation, Int)] = None
}

class Graph(graphControlState: GraphControlState, id: Int)(implicit ctx: Geometry.Context) extends LazyLogging {

  val clientId: String = s"graph-$id"

  val tim = false
  //val tim = true
  val state = new GraphState()

  private def unsubscribe(): Unit = {
    for ((metricName, agg, scale) ← state.subscribedForRT) {
      graphControlState.conf.data.getDataSource.unsubscribe(metricName, agg, scale, clientId)
      state.subscribedForRT = None
    }
  }

  private def subscribe(scale: Int): Unit = {
    val metricName = graphControlState.metric.get.name
    val aggregation = graphControlState.aggregation.get
    val sub = (metricName, aggregation, scale)
    if (!state.subscribedForRT.contains(sub)) {
      unsubscribe()
      graphControlState.conf.data.getDataSource.subscribe(metricName, aggregation, scale, clientId)
      state.subscribedForRT = Some(sub)
    }
  }

  def reset(): Unit = {
    state.subscribedForRT = None
  }

  def resetState(): Unit = {
    unsubscribe()
    state.dataView = None
    state.bestScale = 0
    state.drawScale = 0
    state.drawAdjCurrent = 0.0
    state.drawDataIncarnation = -1L
  }

  def isInteresting(req: Protocol.WSHistRequest): Boolean = {
    val res = graphControlState.metric.exists(_.name == req.metric) &&
      graphControlState.aggregation.contains(req.aggregation) &&
      state.bestScale <= req.scale &&
      req.scale <= state.drawScale &&
      Protocol.Interval(ctx.getState.gmin.x, ctx.getState.gmax.x).isIntersected(req.intervals)
    //    println(s"res=$res, graphControlState.metric=${graphControlState.metric.get.name}, req.metric=${req.metric}, " +
    //      s"graphControlState.aggregation=${graphControlState.aggregation.get.toString}, " +
    //      s"req.aggregation=${req.aggregation.toString} " +
    //      s"state.bestScale=${state.bestScale}, req.scale=${req.scale}, state.drawScale=${state.drawScale}, " +
    //      s"Protocol.Interval(ctx.getState.gmin.x, ctx.getState.gmax.x).isIntersected(req.intervals)=${Protocol.Interval(ctx.getState.gmin.x, ctx.getState.gmax.x).isIntersected(req.intervals)} " +
    //      s"Protocol.Interval(ctx.getState.gmin.x, ctx.getState.gmax.x)=${Protocol.Interval(ctx.getState.gmin.x, ctx.getState.gmax.x)}, " +
    //      s"req.intervals=${req.intervals}")
    res
  }

  def isNewCurrentInteresting: Boolean = {
    val currentOnScreen = ctx.getState.gmin.x - ctx.getState.step <= ctx.getState.current && ctx.getState.current <= ctx.getState.gmax.x + ctx.getState.step
    val adjCurrentChanged = state.drawAdjCurrent != ctx.getState.adjCurrent
    val dataIncarnationChanged = state.drawDataIncarnation != graphControlState.metricData.map(_.incarnation).getOrElse(-1L)
    val isRealTime = ctx.getState.coordinates == Geometry.CoordinatesUniverseRealTime || ctx.getState.gridCoordinates == Geometry.CoordinatesUniverseRealTime
    val res = (currentOnScreen && (adjCurrentChanged || dataIncarnationChanged)) || (adjCurrentChanged && isRealTime)
    //println(s"state.drawAdjCurrent=${state.drawAdjCurrent}, ctx.getState.adjCurrent=${ctx.getState.adjCurrent}")
    //println(s"isNewCurrentInteresting=$res, currentOnScreen=$currentOnScreen, adjCurrentChanged=$adjCurrentChanged, dataIncarnationChanged=$dataIncarnationChanged, isRealTime=$isRealTime")
    res
  }

  /**
   * drawCombined: to draw multiple aggregations on he same graph. todo
   */
  // todo: make selectavle list of aggregations
  // todo: use multiple aggregation for interesting checking
  // todo: check details for Info ?
  // todo: set dataView ?
  def drawCombined(): Unit = graphControlState.metricData match {
    case Some(metricData) ⇒
      val aggs = Seq(
        Aggregations.Min,
        Aggregations.PercentileCont(0.01),
        Aggregations.PercentileCont(0.05),
        Aggregations.PercentileCont(0.10),
        Aggregations.PercentileCont(0.25),
        Aggregations.PercentileCont(0.50),
        Aggregations.PercentileCont(0.75),
        Aggregations.PercentileCont(0.90),
        Aggregations.PercentileCont(0.95),
        Aggregations.PercentileCont(0.99),
        Aggregations.Max)
      val combAggData = metricData.getCombined(1, aggs)
      val metricStep = metricData.getMetricConf.step
      val targetScale = (ctx.scale * state.minPTP / metricStep).toInt
      val combDataView = combAggData.get(targetScale, ctx.getState.gmin.x, ctx.getState.gmax.x)

      ctx.transform.setStep(combDataView.step)
      //state.dataView = Some(dataView)
      state.bestScale = combDataView.bestScale
      state.drawScale = (combDataView.step / metricStep).toInt
      state.drawAdjCurrent = ctx.getState.adjCurrent
      state.drawDataIncarnation = metricData.incarnation

      val hiddenObjects = graphControlState.hiddenObjects
      val transform: Double ⇒ Double = graphControlState.transform(state)
      val objectsOrder = graphControlState.order

      val simpleData = combDataView.simple(hiddenObjects, transform)
      setByRange(simpleData.range)
      drawCombinedLineChart(simpleData, metricData)

    case None ⇒
  }

  def draw(): Unit = {
    //drawCombined()
    drawRegular()
  }

  def drawRegular(): Unit = graphControlState.metricData match {
    case Some(metricData) ⇒
      graphControlState.aggregationData match {
        case Some(aggregationData) ⇒
          val metricStep = metricData.getMetricConf.step
          val targetScale = (ctx.scale * state.minPTP / metricStep).toInt

          val dataView = if (tim) {
            timing("data.get") { aggregationData.get(targetScale, ctx.getState.gmin.x, ctx.getState.gmax.x) }
          } else {
            aggregationData.get(targetScale, ctx.getState.gmin.x, ctx.getState.gmax.x)
          }

          ctx.transform.setStep(dataView.step)
          state.dataView = Some(dataView)
          state.bestScale = dataView.bestScale
          state.drawScale = (dataView.step / metricStep).toInt
          state.drawAdjCurrent = ctx.getState.adjCurrent
          state.drawDataIncarnation = metricData.incarnation

          //println(s"targetScale=$targetScale, ctx.getState.gmin=${ctx.getState.gmin}, ctx.getState.gmax=${ctx.getState.gmax}, ctx.getState.adjCurrent=${ctx.getState.adjCurrent}")

          dataView.info match {
            case info: CacheScale.CacheScaleInfo ⇒
              if (info.needSubscribe(ctx.getState.gmin.x, ctx.getState.gmax.x)) {
                subscribe(state.drawScale)
              } else {
                unsubscribe()
              }
            case _ ⇒
              unsubscribe()
          }

          val hiddenObjects = graphControlState.hiddenObjects
          val transform: Double ⇒ Double = graphControlState.transform(state)
          val objectsOrder = graphControlState.order

          graphControlState.chart match {
            case Charts.LineChart ⇒
              if (tim) {
                val lineData = timing("dataView.line") { dataView.line(hiddenObjects, transform) }
                setByRange(lineData.range)
                timing("drawLineChart") { drawLineChart(lineData, metricData) }
              } else {
                val lineData = dataView.line(hiddenObjects, transform)
                setByRange(lineData.range)
                drawLineChart(lineData, metricData)
              }
            case Charts.LayeredAreaChart ⇒
              if (tim) {
                val lineData = timing("dataView.line") { dataView.line(hiddenObjects, transform) }
                setByRange(lineData.range)
                timing("drawLayeredAreaChart") { drawLayeredAreaChart(lineData, metricData, objectsOrder) }
              } else {
                val lineData = dataView.line(hiddenObjects, transform)
                setByRange(lineData.range)
                drawLayeredAreaChart(lineData, metricData, objectsOrder)
              }
            case Charts.StackedAreaChart ⇒
              if (tim) {
                val stackedData = timing("dataView.stacked") { dataView.stacked(objectsOrder, hiddenObjects, transform) }
                setByRange(stackedData.range)
                timing("drawStackedAreaChart") { drawStackedAreaChart(stackedData, metricData, objectsOrder) }
              } else {
                val stackedData = dataView.stacked(objectsOrder, hiddenObjects, transform)
                setByRange(stackedData.range)
                drawStackedAreaChart(stackedData, metricData, objectsOrder)
              }
            case Charts.SimpleLineChart ⇒
              if (tim) {
                val simpleData = timing("dataView.line") { dataView.simple(hiddenObjects, transform) }
                setByRange(simpleData.range)
                timing("drawLineChart") { drawSimpleLineChart(simpleData, metricData) }
              } else {
                val simpleData = dataView.simple(hiddenObjects, transform)
                setByRange(simpleData.range)
                drawSimpleLineChart(simpleData, metricData)
              }
          }
          if (state.drawTicks) {
            drawTicks(dataView, aggregationData)
          }
        case None ⇒
          logger.info(s"Metric set to ${metricData.getMetricConf.name}, aggregationData is not set")
      }
    case None ⇒
      logger.info("Metric is not set")
  }

  private def noDataDraw(b: Boolean): Unit = if (b) {
    // todo: check if data is loading notify that!
    val pa = ctx.c.size - ctx.getState.p0 - ctx.getState.p1
    val pb = V(ctx.getState.p0.x, ctx.getState.p1.y)
    val font = Font(px = pa.y * 0.8 min ctx.c.size.x / 6, fillStyle = "rgba(0,100,170,0.5)")
    CP(pa / 2 + pb).text("NO DATA", font)
  }

  private def setByRange(range: Option[DataView.Range]): Unit = {
    range.foreach {
      case DataView.Range(min0, max0) ⇒
        val (min1, max1) = if (graphControlState.fromZero) (0.0, max0) else (min0, max0)
        if (min1 != max1) {
          val delta = (max1 - min1) * 0.05
          val min2 = if (min1 == 0.0) 0.0 else min1 - delta
          val max2 = if (max1 == 0.0) 1.0 else max1 + delta
          if (ctx.getState.gmax.y != max2 || ctx.getState.gmin.y != min2) {
            ctx.transform.setY(min2, max2)
          }
        }
    }
  }

  def drawLineChart(lineData: LineData, metricData: MetricData): Unit = {
    var noDataShown = true
    val (kx, ky, bx, by) = getCoefficients
    lineData.data.forEach { (points, objId) ⇒
      val obj = metricData.getObjectById(objId)
      directBeginPath
      points.forEach { p ⇒
        val xCanvas = kx * p.key + bx
        val yCanvas = ky * p.value + by
        if (noDataShown || p.isAppearing) {
          noDataShown = false
          ctx.c.brush.moveTo(xCanvas, yCanvas)
          if (p.isAppearing && p.isDisappearing) {
            ctx.c.brush.arc(xCanvas, yCanvas, 1.5, 0, 2 * math.Pi)
          }
        } else {
          ctx.c.brush.lineTo(xCanvas, yCanvas)
        }
      }
      directStroke(obj.getColor1, 3)
    }
    noDataDraw(noDataShown)
  }

  def drawSimpleLineChart(simpleData: SimpleData, metricData: MetricData): Unit = {
    var noDataShown = true
    val (kx, ky, bx, by) = getCoefficients
    simpleData.data.forEach { (points, objId) ⇒
      val obj = metricData.getObjectById(objId)
      directBeginPath
      var first = true
      points.forEach { p ⇒
        val xCanvas = kx * p.key + bx
        val yCanvas = ky * p.value + by
        if (first) {
          noDataShown = false
          ctx.c.brush.moveTo(xCanvas, yCanvas)
          first = false
        } else {
          ctx.c.brush.lineTo(xCanvas, yCanvas)
        }
      }
      directStroke(obj.getColor1, 3)
    }
    noDataDraw(noDataShown)
  }

  def drawCombinedLineChart(simpleData: SimpleData, metricData: MetricData): Unit = {
    var noDataShown = true
    val (kx, ky, bx, by) = getCoefficients
    simpleData.data.forEach { (points, objId) ⇒
      val color = plutarch.shared.colors.Default.list1000(objId)
      directBeginPath
      var first = true
      points.forEach { p ⇒
        val xCanvas = kx * p.key + bx
        val yCanvas = ky * p.value + by
        if (first) {
          noDataShown = false
          ctx.c.brush.moveTo(xCanvas, yCanvas)
          first = false
        } else {
          ctx.c.brush.lineTo(xCanvas, yCanvas)
        }
      }
      directStroke(color, 1)
    }
    noDataDraw(noDataShown)
  }

  def drawLayeredAreaChart(lineData: LineData, metricData: MetricData, objectsOrder: Int ⇒ Int): Unit = {
    var noDataShown = true
    val (kx, ky, bx, by) = getCoefficients
    val orderedObjects = JSArray.empty[Int]
    lineData.data.forEach { (_, k) ⇒
      orderedObjects.push(k)
    }
    orderedObjects.sort((id1, id2) ⇒ objectsOrder(id2).compareTo(objectsOrder(id1))).forEach { objId ⇒
      val points = lineData.data.get(objId).asInstanceOf[LDP]
      if (points.length > 0) {
        val obj = metricData.getObjectById(objId)
        var first = -1.0
        var last = -1.0
        var minY = Double.MaxValue
        directBeginPath
        points.forEach { p ⇒
          val xCanvas = kx * p.key + bx
          last = xCanvas
          val yCanvas = ky * p.value + by
          minY = minY min yCanvas
          if (first == -1) {
            first = xCanvas
            ctx.c.brush.moveTo(xCanvas, by)
            noDataShown = false
          }
          if (!p.isAppearing && !p.isDisappearing) {
            ctx.c.brush.lineTo(xCanvas, yCanvas)
          } else if (p.isAppearing && p.isDisappearing) {
            ctx.c.brush.lineTo(xCanvas, by)
            ctx.c.brush.lineTo(xCanvas, yCanvas)
            ctx.c.brush.lineTo(xCanvas, by)
          } else if (p.isAppearing) {
            ctx.c.brush.lineTo(xCanvas, by)
            ctx.c.brush.lineTo(xCanvas, yCanvas)
          } else {
            ctx.c.brush.lineTo(xCanvas, yCanvas)
            ctx.c.brush.lineTo(xCanvas, by)
          }
        }
        ctx.c.brush.lineTo(last, by)
        ctx.c.brush.lineTo(first, by)
        val gradient = ctx.c.brush.createLinearGradient(0, minY, 0, by)
        gradient.addColorStop(0, Colors.toRGBA(obj.getColor1, state.alpha))
        gradient.addColorStop(1, Colors.toRGBA(obj.getColor2, state.alpha))
        directFillAndStroke(gradient, obj.getColor3)
      }
    }
    noDataDraw(noDataShown)
  }

  def drawStackedAreaChart(stackedData: StackedData, metricData: MetricData, objectsOrder: Int ⇒ Int): Unit = {
    var noDataShown = true
    val (kx, ky, bx, by) = getCoefficients
    val orderedObjects = JSArray.empty[Int]
    stackedData.data.forEach { (_, k) ⇒
      orderedObjects.push(k)
    }
    orderedObjects.sort((id1, id2) ⇒ objectsOrder(id2).compareTo(objectsOrder(id1))).forEach { objId ⇒
      val points = stackedData.data.get(objId).asInstanceOf[DP]
      if (points.length > 0) {
        val obj = metricData.getObjectById(objId)
        var first = -1.0
        var last = -1.0
        var minY = Double.MaxValue
        directBeginPath
        points.forEach { p ⇒
          val xCanvas = kx * p.key + bx
          last = xCanvas
          val yCanvas = ky * p.value + by
          minY = minY min yCanvas
          if (first == -1) {
            first = xCanvas
            ctx.c.brush.moveTo(xCanvas, by)
            noDataShown = false
          }
          ctx.c.brush.lineTo(xCanvas, yCanvas)
        }
        ctx.c.brush.lineTo(last, by)
        ctx.c.brush.lineTo(first, by)
        val gradient = ctx.c.brush.createLinearGradient(0, minY, 0, by)
        gradient.addColorStop(0, obj.getColor1)
        gradient.addColorStop(1, obj.getColor2)
        directFillAndStroke(gradient, obj.getColor3)
      }
    }
    noDataDraw(noDataShown)
  }

  private def drawTicks(dataView: DataView, aggregationData: AggregationData): Unit = {
    val ref = ctx.getState.p0.y / 16
    val metric = graphControlState.metric.get
    val (kx, ky, bx, by) = getCoefficients
    val scaleCnt = metric.scales.size
    val scaleOrd = metric.scales.sorted.zipWithIndex.map(x ⇒ x._1 -> x._2.toDouble / scaleCnt).toMap
    def scaleToYCanvas(scale: Int): Double = {
      //val delta = 1.0 / 22.0 * ctx.getState.nsize.y
      //val y = ctx.getState.gmin.y + 10.0 / 11.0 * ctx.getState.gsize.y * Math.log(scale) / Math.log(metric.scales.last)
      //ky * y + by - delta
      val y = ctx.getState.gmin.y + 0.95 * ctx.getState.gsize.y * (scaleOrd(scale) + 0.025)
      ky * y + by
    }
    val yCanvas = scaleToYCanvas(state.drawScale)
    val xCanvasRight = kx * dataView.reqRight + bx
    var key = dataView.left
    directBeginPath
    ctx.c.brush.moveTo(kx * key + bx, yCanvas)
    ctx.c.brush.lineTo(kx * (dataView.right - dataView.step) + bx, yCanvas)
    while (key < dataView.right) {
      val xCanvas = kx * key + bx
      ctx.c.brush.moveTo(xCanvas, yCanvas)
      ctx.c.brush.lineTo(xCanvas, yCanvas - ref)
      key += dataView.step
    }
    directStroke()

    val stepName = DateTools.toTimeStrAuto(dataView.step)
    val total = (dataView.right - dataView.left) / dataView.step

    ctx.c.brush.font = s"${3 * ref}px Verdana"
    ctx.c.brush.fillStyle = "black"
    ctx.c.brush.textAlign = "right"
    ctx.c.brush.textBaseline = "bottom"
    ctx.c.brush.fillText(total.toString, xCanvasRight, yCanvas - ref)
    ctx.c.brush.textBaseline = "top"
    ctx.c.brush.fillText(stepName, xCanvasRight, yCanvas)

    dataView.info match {
      case CacheScale.CacheScaleInfo(cacheScaleState) ⇒
        val wm = kx * cacheScaleState.watermark + bx
        val cur = kx * cacheScaleState.current + bx
        directBeginPath
        ctx.c.brush.moveTo(wm, yCanvas)
        ctx.c.brush.lineTo(wm, yCanvas - 3 * ref)
        directStroke("red", 2)

        directBeginPath
        ctx.c.brush.moveTo(cur, yCanvas)
        ctx.c.brush.lineTo(cur, yCanvas - 3 * ref)
        directStroke("green", 2)

        directBeginPath
        ctx.c.brush.moveTo(wm, yCanvas)
        ctx.c.brush.lineTo(cur, yCanvas)
        directStroke()

        cacheScaleState.lastInterval match {
          case Some(MInterval(left, right, _)) ⇒
            val x1 = kx * left + bx
            val x2 = kx * right + bx

            directBeginPath
            ctx.c.brush.moveTo(x1, yCanvas)
            ctx.c.brush.lineTo(x1, yCanvas + 3 * ref)
            directStroke("blue")

            directBeginPath
            ctx.c.brush.moveTo(x2, yCanvas)
            ctx.c.brush.lineTo(x2, yCanvas + 3 * ref)
            directStroke("blue")

          case _ ⇒
        }
      case _ ⇒
    }

    def drawMaxAvailable(): Unit = {
      aggregationData match {
        case aggdata: CacheAggregationData ⇒
          val frameInterval = Interval(ctx.getState.gmin.x, ctx.getState.gmax.x)
          val intervalScales = for {
            (scale, intervals) ← aggdata.getAll
            if scale <= state.drawScale
            interval ← intervals
            if interval.isIntersected(frameInterval)
          } yield {
            (scale, interval)
          }
          val byLeft = intervalScales.map(x ⇒ x._2.left -> x).toList.groupBy(_._1).map(x ⇒ (x._1, x._2.map(_._2)))
          val byRight = intervalScales.map(x ⇒ x._2.right -> x).toList.groupBy(_._1).map(x ⇒ (x._1, x._2.map(_._2)))
          val points = intervalScales.flatMap(x ⇒ Seq(x._2.left, x._2.right)).toList.distinct.sorted
          var curScale = Option.empty[Int]
          var curIntervals = Set.empty[(Int, Interval)]
          val lb = ListBuffer.empty[(Double, Option[Double])]

          for (point ← points) {
            curIntervals ++= byLeft.getOrElse(point, Nil)
            curIntervals --= byRight.getOrElse(point, Nil)
            if (curIntervals.nonEmpty) {
              val thisValue = curIntervals.map(_._1).min
              if (curScale.isEmpty || curScale.get != thisValue) {
                curScale = Some(thisValue)
                lb += (kx * point + bx -> curScale.map(scaleToYCanvas))
              }
            } else {
              curScale = None
              lb += (kx * point + bx -> None)
            }
          }

          beginPath
          var prevOptY = Option.empty[Double]
          for ((x, optY) ← lb.result()) {
            prevOptY match {
              case Some(prevY) ⇒
                optY match {
                  case Some(y) ⇒
                    ctx.c.brush.lineTo(x, prevY)
                    ctx.c.brush.lineTo(x, y)
                  case None ⇒
                    ctx.c.brush.lineTo(x, prevY)
                }
              case None ⇒
                optY match {
                  case Some(y) ⇒
                    ctx.c.brush.moveTo(x, y)
                  case None ⇒
                }
            }
            prevOptY = optY
          }
          directStroke("#00FF00", 3)

        case _ ⇒
      }
    }

    def drawAllAvailable(): Unit = {
      aggregationData match {
        case aggdata: CacheAggregationData ⇒
          beginPath
          val frameInterval = Interval(ctx.getState.gmin.x, ctx.getState.gmax.x)
          for {
            (scale, intervals) ← aggdata.getAll
            interval ← intervals
            if interval.isIntersected(frameInterval)
          } {
            val y = scaleToYCanvas(scale)
            val (x1, x2) = (kx * interval.left + bx, kx * interval.right + bx)
            if (x2 - x1 >= 5) {
              ctx.c.brush.moveTo(x1, y)
              ctx.c.brush.lineTo(x2, y)
            } else {
              val x = (x1 + x2) / 2
              ctx.c.brush.moveTo(x, y)
              ctx.c.brush.arc(x, y, 1.5, 0, 2 * math.Pi)
            }
          }
          directStroke("rgba(0,200,0,0.6)", 1)
        case _ ⇒
      }
    }

    def drawAllRequested(): Unit = {
      aggregationData match {
        case aggdata: CacheAggregationData ⇒
          beginPath
          val frameInterval = Interval(ctx.getState.gmin.x, ctx.getState.gmax.x)
          for {
            (scale, intervals) ← aggdata.getRequested
            interval ← intervals
            if interval.isIntersected(frameInterval)
          } {
            val y = scaleToYCanvas(scale)
            val (x1, x2) = (kx * interval.left + bx, kx * interval.right + bx)
            if (x2 - x1 >= 5) {
              ctx.c.brush.moveTo(x1, y)
              ctx.c.brush.lineTo(x2, y)
            } else {
              val x = (x1 + x2) / 2
              ctx.c.brush.moveTo(x, y)
              ctx.c.brush.arc(x, y, 1.5, 0, 2 * math.Pi)
            }
          }
          directStroke("rgba(200,0,0,0.3)", 2)
        case _ ⇒
      }
    }

    drawAllAvailable()
    drawMaxAvailable()
    drawAllRequested()
  }

}
