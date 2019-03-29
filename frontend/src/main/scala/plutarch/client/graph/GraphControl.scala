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

package plutarch.client.graph

import Geometry.V
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Div
import org.scalajs.dom.svg.SVG
import plutarch.client.data.{ AggregationData, Data, DataControl, MetricData }
import plutarch.client.experemental.Tools
import plutarch.shared.Protocol
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Charts
import plutarch.shared.data.metrics.Conf
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags
import slogging.LazyLogging

trait GraphControlConf {
  def data: Data
  def drawThrottle: Int
  def mouseThrottle: Int
  def clickDelay: Int
  def autoResize: Boolean
  def contextInitConf: Geometry.ContextInitConf
  def contextLimitsConf: Geometry.ContextLimitsConf
  def selectingConf: SelectingConf
}

class GraphControlState(val conf: GraphControlConf) {
  var metric: Option[Conf] = None
  var aggregation: Option[Aggregation] = None
  var metricData: Option[MetricData] = None
  var aggregationData: Option[AggregationData] = None
  var transform: GraphState ⇒ Double ⇒ Double = (_: GraphState) ⇒ (x: Double) ⇒ x
  var fromZero: Boolean = false
  var chart: Charts.Chart = Charts.LineChart
  var order: Int ⇒ Int = x ⇒ x
  var hiddenObjects: Set[Int] = Set.empty
}

abstract class GraphControl(conf: GraphControlConf) extends LazyLogging {
  graphControl ⇒

  def draw(): Unit

  val state = new GraphControlState(conf)

  private def controlUnset(): Unit = {
    if (state.metric.isDefined && state.aggregation.isDefined) {
      conf.data.getControl.unset(state.metric.get.name, state.aggregation.get, graphGeometry)
    }
  }

  private def controlSet(): Unit = {
    if (state.metric.isDefined && state.aggregation.isDefined) {
      conf.data.getControl.set(state.metric.get.name, state.aggregation.get, graphGeometry)
    }
  }

  def setMetric(metric: Conf): Unit = {
    graph.resetState()
    controlUnset()
    state.metric = Some(metric)
    state.metricData = Some(conf.data.get(metric.name))
    state.aggregation = None
    state.aggregationData = None
    selecting.initAggs()
  }

  def isMetricSet: Boolean = state.metric.isDefined
  def getMetric: Option[String] = state.metric.map(_.name)

  def setAggregation(aggregation: Aggregation, transform: GraphState ⇒ Double ⇒ Double): Unit = {
    graph.resetState()
    controlUnset()
    state.aggregation = Some(aggregation)
    state.aggregationData = state.metricData.map(_.get(aggregation))
    state.transform = transform
    controlSet()
    drawThrottle.immediate()
  }

  def setMetricAndAggregation(metric: Conf): Unit = {
    setMetric(metric)
    setAggregation(metric.aggregations.head)
  }

  def setAggregation(aggregation: Aggregation): Unit = {
    setAggregation(aggregation, _ ⇒ x ⇒ x)
  }

  def setChart(chart: Charts.Chart): Unit = {
    state.chart = chart
  }

  def setFromZero(fromZero: Boolean): Unit = {
    state.fromZero = fromZero
  }

  private val graphLayer = Geometry.Canv(canvas(position.absolute, left := 0, top := 0, zIndex := -3).render)
  private val selectLayer = Geometry.Canv(canvas(position.absolute, left := 0, top := 0, zIndex := -2).render)
  private val cursorLayer = Geometry.Canv(canvas(position.absolute, left := 0, top := 0, zIndex := -1).render)
  private val topSvgLayer: SVG = svgTags.svg().render
  private val topLayer = div(topSvgLayer, position.absolute, left := 0, top := 0, zIndex := 0).render

  lazy val drawThrottle: Tools.Throttle = Tools.Throttle(conf.drawThrottle) {
    graphLayer.clear()
    selectLayer.clear()
    cursorLayer.clear()

    graph.draw()
    draw() // drawing over the graph

    grid.draw()
    selecting.draw()
    controlCursor.refresh()
  }

  private val onSort = () ⇒ {
    state.order(1) match {
      case 1  ⇒ state.order = x ⇒ -x
      case -1 ⇒ state.order = x ⇒ x
    }
    drawThrottle.immediate()
  }

  implicit val graphGeometry: Geometry.Context = new Geometry.Context(graphLayer)(conf.contextInitConf, conf.contextLimitsConf)
  private val controlCursor = Cursor(cursorLayer)
  private val selecting = Selecting(this, selectLayer, topSvgLayer, drawThrottle, onSort, conf.selectingConf)(graphGeometry)
  private val grid = Grid(state)(graphGeometry)
  private val graph = Graph(state)(graphGeometry)

  val root: Div =
    div(
      position.relative,
      minWidth := 70,
      cursor := "default",
      graphLayer.canv,
      cursorLayer.canv,
      selectLayer.canv,
      topLayer).render

  def getDefaultSize: (Int, Int) = {
    val ratio = Geometry.Canv.getRatio
    val width = dom.document.body.clientWidth
    val screenRatio = dom.window.innerHeight / dom.window.innerWidth
    val h0 = (dom.window.innerHeight / 3).toInt
    val h1 = (dom.window.innerWidth / 2).toInt
    val h2 = (dom.window.innerHeight - 40).toInt
    val height =
      if (dom.window.innerHeight * ratio > 1000) h0.max((666 / ratio).toInt)
      else if (screenRatio > 0.5) h1
      else h2
    (width, height)
  }

  def setSize(): Unit = {
    val (width, height) = getDefaultSize
    setSize(width, height)
  }

  def setCurrent(current: Long): Unit = {
    graphGeometry.transform.setCurrent(current)
    if (graph.isNewCurrentInteresting) drawThrottle.asap()
  }

  def update(req: Protocol.WSHistRequest): Unit = {
    if (graph.isInteresting(req)) drawThrottle.immediate()
  }

  def setSize(width: Int, height: Int): Unit = {
    dom.window.scrollTo(0, 0)

    logger.debug("width x height = {} x {}", width, height)

    val widthpx = s"${width}px"
    val heightpx = s"${height}px"

    //root.style.width = widthpx
    root.style.height = heightpx
    topSvgLayer.style.width = widthpx
    topSvgLayer.style.height = heightpx
    topLayer.style.width = widthpx
    topLayer.style.height = heightpx
    graphGeometry.transform.setCanvSize(width, height)
    selectLayer.set(width, height)
    cursorLayer.set(width, height)

    drawThrottle.immediate()
  }

  def reset(): Unit = {
    graph.reset()
  }

  def click(h: Geometry.H1): Unit = {
    // todo
  }

  /* set event handlers */

  // need to keep all child events
  dom.window.addEventListener("resize", (_: UIEvent) ⇒ if (conf.autoResize) {
    setSize()
  })

  topLayer.onmouseenter = (e: Event) ⇒ {
    e.preventDefault()
  }

  private var lastMouseEvent: MouseEvent = _
  private val mouseThrottle = Tools.Throttle(conf.mouseThrottle) {
    controlCursor.move(lastMouseEvent)
    if (selecting.state > 0) {
      controlCursor.getPos.foreach(selecting.move)
    }
    if (graphGeometry.transform.scrollingState > 0) {
      controlCursor.getPos.foreach(graphGeometry.transform.scrollingMove)
      drawThrottle.asap()
    }
  }

  topLayer.onmousemove = (e: MouseEvent) ⇒ {
    lastMouseEvent = e
    mouseThrottle.asap()
    e.preventDefault()
  }
  topLayer.onmouseout = (e: Event) ⇒ {
    if (graphGeometry.transform.scrollingState > 0) {
      graphGeometry.transform.scrollingStop()
      drawThrottle.asap()
    } else if (selecting.state > 0) {
      selecting.stop()
    }
    mouseThrottle.cancel()
    controlCursor.moveAway()
    e.preventDefault()
  }
  topLayer.onmousedown = (e: MouseEvent) ⇒ {
    if (!selecting.isOnZoom) {
      if (controlCursor.isOnHold) {
        controlCursor.toggleHold()
        controlCursor.move(lastMouseEvent)
      }
      if ((e.buttons & 5) > 0) {
        controlCursor.getPos.foreach { pos ⇒
          graphGeometry.transform.setKeepRight(b = false)
          graphGeometry.transform.scrollingStart(pos)
        }
      } else if ((e.buttons & 2) > 0) {
        controlCursor.getPos.foreach { pos ⇒
          selecting.start(pos)
        }
      }
    }
    e.preventDefault()
  }
  topLayer.onmouseup = (e: MouseEvent) ⇒ {
    if ((e.button == 0 || e.button == 1) && graphGeometry.transform.scrollingState > 0) {
      graphGeometry.transform.scrollingStop()
      drawThrottle.asap()
    } else if (e.button == 2 && selecting.state > 0) {
      selecting.stop()
    }
    e.preventDefault()
  }
  topLayer.onmouseenter = (e: Event) ⇒ {
    e.preventDefault()
  }
  topLayer.oncontextmenu = (e: Event) ⇒ {
    e.preventDefault()
  }
  topLayer.ondblclick = (e: Event) ⇒ {
    if (!selecting.isOnZoom) {
      controlCursor.toggleHold()
    }
    e.preventDefault()
  }
  private var lastClick = 0L
  topLayer.onclick = (e: MouseEvent) ⇒ {
    if (!selecting.isOnZoom) {
      val thisClick = Tools.now()
      if (thisClick - lastClick > conf.clickDelay) {
        controlCursor.getPos.foreach { pos ⇒
          click(pos)
        }
      }
      lastClick = thisClick
    }
    e.preventDefault()
  }

  topLayer.addEventListener("wheel", (e: WheelEvent) ⇒ {
    controlCursor.getPos.foreach { pos ⇒
      graphGeometry.transform.rescale(pos, e.deltaY)
      drawThrottle.asap()
    }
    e.preventDefault()
  })

  // touch events
  topLayer.addEventListener("touchstart", (ev: TouchEvent) ⇒ handleStart(ev), useCapture = false)
  topLayer.addEventListener("touchend", (ev: TouchEvent) ⇒ handleEnd(ev), useCapture = false)
  topLayer.addEventListener("touchcancel", (ev: TouchEvent) ⇒ handleCancel(ev), useCapture = false)
  topLayer.addEventListener("touchmove", (ev: TouchEvent) ⇒ handleMove(ev), useCapture = false)

  private var ongoingTouches = Map.empty[Double, V]
  private var touchScalePrev = Option.empty[Map[Double, V]]
  def handleStart(ev: TouchEvent): Unit = {
    ev.preventDefault()
    val touches = cursorLayer.touchListToCTouchList(ev.changedTouches)
    ongoingTouches ++= touches
    if (ongoingTouches.size == 1) {
      controlCursor.moveTo(ongoingTouches.values.head)
      controlCursor.getPos.foreach { pos ⇒
        graphGeometry.transform.setKeepRight(b = false)
        graphGeometry.transform.scrollingStart(pos)
      }
    } else {
      if (graphGeometry.transform.scrollingState > 0) {
        graphGeometry.transform.scrollingStop()
        drawThrottle.asap()
      }
      if (ongoingTouches.size == 2) {
        touchScalePrev = Some(ongoingTouches)
        controlCursor.moveTo(ongoingTouches.values.reduce(_ + _) / 2)
      }
    }
  }
  def handleEnd(ev: TouchEvent): Unit = {
    ev.preventDefault()
    val touches = cursorLayer.touchListToCTouchList(ev.changedTouches)
    ongoingTouches --= touches.map(_._1)
    // handle case when end of zoom and screen is jumped
    if (graphGeometry.transform.scrollingState > 0) {
      graphGeometry.transform.scrollingStop()
      drawThrottle.asap()
    }
    if (ongoingTouches.size == 1) {
      controlCursor.moveTo(ongoingTouches.values.head)
      controlCursor.getPos.foreach { pos ⇒
        graphGeometry.transform.setKeepRight(b = false)
        graphGeometry.transform.scrollingStart(pos)
      }
    }
    touchScalePrev = None
  }
  def handleCancel(ev: TouchEvent): Unit = {
    ev.preventDefault()
    ongoingTouches = Map.empty
    if (graphGeometry.transform.scrollingState > 0) {
      graphGeometry.transform.scrollingStop()
      drawThrottle.asap()
    }
    touchScalePrev = None
  }
  def handleMove(ev: TouchEvent): Unit = {
    ev.preventDefault()
    val touches = cursorLayer.touchListToCTouchList(ev.changedTouches)
    ongoingTouches ++= touches
    if (ongoingTouches.size == 1) {
      controlCursor.moveTo(ongoingTouches.values.head)
      if (graphGeometry.transform.scrollingState > 0) {
        controlCursor.getPos.foreach(graphGeometry.transform.scrollingMove)
        if (graphGeometry.transform.scrollingState > 1) {
          drawThrottle.asap()
        }
      }
    } else if (ongoingTouches.size == 2) touchScalePrev match {
      case None ⇒
      case Some(prev) ⇒
        val List((id1, p1), (id2, p2)) = prev.toList
        val r1 = ongoingTouches(id1)
        val r2 = ongoingTouches(id2)
        graphGeometry.transform.touchZoom(p1, p2, r1, r2)
        touchScalePrev = Some(ongoingTouches)
        controlCursor.moveTo(ongoingTouches.values.reduce(_ + _) / 2)
        drawThrottle.asap()
    }
  }

  // init draw
  drawThrottle.later()
}
