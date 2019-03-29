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

import org.scalajs.dom.svg.SVG
import plutarch.client.experemental.Tools.{ InnerText, innerText }
import plutarch.client.experemental.{ DateTools, Signal, SignalSource, Tools }
import plutarch.shared.data.{ Aggregations, Charts }

import scala.concurrent.duration._

/*
* Disclaimer: the copy/paste technique used a lot here. This part is still being prototyping a lot.
* */

object Selecting {
  def apply(
    graphControl: GraphControl,
    canv:         Geometry.Canv,
    svgLayer:     SVG,
    drawThrottle: Tools.Throttle,
    onSort:       () ⇒ Unit,
    conf:         SelectingConf)(thatCtx: Geometry.Context): Selecting =
    new Selecting(graphControl, canv, svgLayer, drawThrottle, onSort, conf)(thatCtx)

  private class SelectingState() {
    var interval: (Option[Double], Option[Double], Boolean) = (None, None, false)
    var curTop: Option[(Geometry.V, Geometry.V)] = None
    var selected: Boolean = false
    var state: Int = 0
    var start: Geometry.H1 = _
    var current: Geometry.H1 = _
    var ratioToMax: Double = 0.0
    var frames = List.empty[((Double, Double), Option[(Geometry.H1, Geometry.H1)])]
    var aggs = List.empty[SVG]
  }

  private object Common {
    def R(ref: Double): Double = 5 * ref
    def X(ref: Double): Double = 30 * ref
    def BackX(ref: Double): Double = 60 * ref
    def Y(ref: Double): Double = 27 * ref
    def BackTX(ref: Double): Double = 30 * ref
    def ZoomTX(ref: Double): Double = -22 * ref
    def SortTX(ref: Double): Double = -19 * ref
    def JumpTX(ref: Double): Double = 15 * ref
    def TY(ref: Double): Double = 19 * ref
    def F(ref: Double): Double = 14 * ref
    val blue = "rgba(30,100,205,0.7)"
    val yellow = "rgba(170,190,25,0.7)"
    val green = "rgba(130,205,140,0.7)"
    val red = "rgba(205,130,130,0.7)"
    val arrow: InnerText = innerText("""\uf060""")
    val times: InnerText = innerText("""\uf00d""")
    val lens: InnerText = innerText("""\uf002""")
    val sort: InnerText = innerText("""\uf0dc""")
  }

  val jumpIntervals = Seq(
    (0, "1m", (t: Long) ⇒ t - 1.minute.toMillis),
    (1, "10m", (t: Long) ⇒ t - 10.minute.toMillis),
    (2, "1h", (t: Long) ⇒ t - 1.hour.toMillis),
    (3, "6h", (t: Long) ⇒ t - 6.hour.toMillis),
    (4, "12h", (t: Long) ⇒ t - 12.hour.toMillis),
    (5, "1d", (t: Long) ⇒ t - 1.day.toMillis),
    (6, "1M", (t: Long) ⇒ DateTools.monthAgo(t)),
    (7, "1Y", (t: Long) ⇒ DateTools.yearAgo(t)),
    (8, "10Y", (t: Long) ⇒ DateTools.yearAgo(t, 10)),
    (9, "50Y", (t: Long) ⇒ DateTools.yearAgo(t, 50)))
}

trait SelectingConf {
  def minSize: Double
  def minChangePixels: Int
  def maxSelectSize: Double
}

class Selecting(
    graphControl: GraphControl,
    canv:         Geometry.Canv,
    svgLayer:     SVG,
    drawThrottle: Tools.Throttle,
    onSort:       () ⇒ Unit,
    conf:         SelectingConf)(implicit thatCtx: Geometry.Context) {

  private val selectingState = new Selecting.SelectingState()

  def state: Int = selectingState.state
  def isSelected: Boolean = selectingState.selected

  def getSelected: (Option[Double], Option[Double], Boolean) = selectingState.interval

  private def update(): Unit = if (selectingState.selected) {
    val x1 = selectingState.start.asUniverse.x
    val x2 = selectingState.current.asUniverse.x
    val (left, right) = if (x1 <= x2) (x1, x2) else (x2, x1)
    selectingState.interval = (Some(left), Some(right), selectingState.ratioToMax > 1)
  } else {
    selectingState.interval = (None, None, false)
  }

  def start(pos: Geometry.H1): Unit = {
    selectingState.state = 1
    selectingState.selected = false
    selectingState.start = pos.to(Geometry.CoordinatesUniverse)
    selectingState.curTop = None
    update()
    if (Variables.backHistory.value.contains(0)) Variables.backHistory.update(-1)
    clear()
  }

  def move(pos: Geometry.H1): Unit = if (selectingState.state > 0) {
    val curPos = pos.to(Geometry.CoordinatesUniverse)
    val disth = curPos - selectingState.start
    val distc = Math.abs(disth.as(Geometry.CoordinatesFrame).x)
    val dist = disth.asUniverse.x
    if (distc > conf.minChangePixels) {
      selectingState.state = 2
      selectingState.selected = true
      selectingState.ratioToMax = Math.abs(dist / conf.maxSelectSize)
      if (selectingState.ratioToMax <= 1) {
        selectingState.current = curPos
      } else {
        selectingState.current = selectingState.start + Geometry.GV(Math.signum(dist) * conf.maxSelectSize, 0)
      }
      update()
      draw()
    }
  }

  def stop(): Unit = {
    selectingState.interval match {
      case (Some(p1), Some(p2), _) ⇒ selectingState.selected = p1 != p2
      case _                       ⇒ selectingState.selected = false
    }
    if (selectingState.selected) {
      if (Variables.backHistory.value.contains(-1)) Variables.backHistory.update(0)
    }
    selectingState.state = 0
  }

  def reset(): Unit = {
    selectingState.state = 0
    selectingState.selected = false
    update()
    selectingState.curTop = None
    clear()
  }

  def set(x: Geometry.H1, y: Geometry.H1): Unit = {
    selectingState.selected = true
    selectingState.start = x
    selectingState.current = y
    update()
    stop()
  }

  def zoom(): Unit = {
    selectingState.interval match {
      case (Some(x), Some(y), _) ⇒
        selectingState.frames = ((thatCtx.getState.gmin.x, thatCtx.getState.gmax.x), Some((selectingState.start, selectingState.current))) :: selectingState.frames
        Variables.backHistory.update(selectingState.frames.size)
        thatCtx.transform.setX(x, y)
      case _ ⇒
    }
  }

  def jump(jumpF: Long ⇒ Long): Unit = {
    val right = thatCtx.getState.gmax.x
    val left = thatCtx.getState.gmin.x
    val newLeft = jumpF(right.toLong)
    if (math.abs(left - newLeft) / (right - left) > 0.01) {
      selectingState.frames = ((left, right), None) :: selectingState.frames
      Variables.backHistory.update(selectingState.frames.size)
      thatCtx.transform.setX(newLeft, right)
    }
  }

  def zoomBack(): Unit = {
    if (selectingState.frames.nonEmpty) {
      val ((x, y), opt) = selectingState.frames.head
      selectingState.frames = selectingState.frames.tail
      Variables.backHistory.update(selectingState.frames.size)
      thatCtx.transform.setX(x, y)
      opt.foreach(s ⇒ set(s._1, s._2))
    } else {
      if (Variables.backHistory.value.contains(0)) {
        reset()
      }
      Variables.backHistory.update(-1)
    }
  }

  def draw(): Unit = {
    if (selectingState.selected) {
      clear()
      val p0 = selectingState.start.prjX(Geometry.CoordinatesFrame)
      val p1 = selectingState.current.prjX(Geometry.CoordinatesFrame) + Geometry.NV(thatCtx.getState.nsize.prjY)
      val fp0 = p0.toFrameOnCanvas
      val fp1 = p1.toFrameOnCanvas
      canv.brush.fillStyle = calcColor(1.0.min(selectingState.ratioToMax))
      canv.brush.fillRect(fp0.x, fp0.y, fp1.x - fp0.x, fp1.y - fp0.y)

      val rightFrame = thatCtx.getState.nsize.x + thatCtx.getState.p0.x
      val leftFrame = thatCtx.getState.p0.x
      val left = p0.vCanvas.x min p1.vCanvas.x
      val right = p0.vCanvas.x max p1.vCanvas.x
      selectingState.curTop = if (left < rightFrame && right > leftFrame) Some((Geometry.V(left max leftFrame, fp1.y), Geometry.V(right min rightFrame, fp1.y))) else None
    }
    refreshSvg()
  }

  private def calcColor(x: Double): String = {
    def c(c0: Double, c1: Double): Double = c0 * (1 - x) + c1 * x
    s"rgba(${c(0, 150).toInt},${c(100, 0).toInt},${c(100, 0).toInt},${c(0.3, 0.6)})"
  }

  private def clear(): Unit = {
    canv.brush.clearRect(0, 0, canv.size.x, canv.size.y)
    selectingState.curTop = None
    refreshSvg()
  }

  // sub controls
  private var _isOnZoom = false
  def isOnZoom: Boolean = _isOnZoom
  def ifZoomPossible: Boolean = {
    selectingState.interval match {
      case (Some(x), Some(y), _) ⇒ y - x > conf.minSize
      case _                     ⇒ false
    }
  }

  private def refreshSvg(): Unit = {
    val cr = Geometry.Canv.getRatio
    val ref = thatCtx.getState.canvSize.y / 350

    val r = Selecting.Common.R(ref)
    val x = Selecting.Common.X(ref)
    val bx = Selecting.Common.BackX(ref)
    val y = Selecting.Common.Y(ref)
    val ztx = Selecting.Common.ZoomTX(ref)
    val btx = Selecting.Common.BackTX(ref)
    val stx = Selecting.Common.SortTX(ref)
    val jtx = Selecting.Common.JumpTX(ref)
    val ty = Selecting.Common.TY(ref)
    val f = Selecting.Common.F(ref)

    Variables.R.update(r / cr)
    Variables.X.update(x / cr)
    Variables.BX.update(bx / cr)
    Variables.Y.update(y / cr)
    Variables.ZTX.update(ztx / cr)
    Variables.BTX.update(btx / cr)
    Variables.STX.update(stx / cr)
    Variables.JTX.update(jtx / cr)
    Variables.TY.update(ty / cr)
    Variables.F.update(f / cr)
    Variables.P0X.update(thatCtx.getState.p0.x / cr)
    Variables.P1Y.update(thatCtx.getState.p1.y / cr)

    val jumpTop = thatCtx.getState.nsize.x + thatCtx.getState.p0.x
    val jumpsTop = Selecting.jumpIntervals.map { case (idx, _, _) ⇒ idx -> (jumpTop - (x + r) * (idx + 1)) / cr }.toMap
    Variables.jumpsTopX.update(jumpsTop)
    Variables.topY.update((thatCtx.getState.p1.y - y - r) / cr)
    Variables.realX.update(thatCtx.getState.p0.x / cr)
    Variables.gridX.update((thatCtx.getState.p0.x + x + r) / cr)
    Variables.chartX.update((thatCtx.getState.p0.x + 2 * (x + r)) / cr)
    Variables.zeroX.update((thatCtx.getState.p0.x + 3 * (x + r)) / cr)
    val aggsTop = (thatCtx.getState.p0.x + 4 * (x + r)) / cr
    val aggsTopX = graphControl.state.metric.toList.flatMap { mconf ⇒
      val aggIdxs = mconf.aggregations.size + (if (mconf.aggregations.contains(Aggregations.Sum)) 1 else 0)
      (0 until aggIdxs).map { idx ⇒ idx -> (aggsTop + (bx + r) * (idx + 1) / cr) }
    }
    Variables.aggsTopX.update(aggsTopX.toMap)
    Variables.aggsTopY.update((thatCtx.getState.p1.y - y - r) / cr)
    Variables.aggsR.update(r / cr)
    Variables.aggsX.update(2 * x / cr)
    Variables.aggsY.update(y / cr)
    Variables.aggsF.update(f / cr)
    Variables.aggsTX.update(x / cr)
    Variables.aggsTY.update(ty / cr)

    selectingState.curTop match {
      case Some((v1, v2)) ⇒

        // zoom
        // v1 and v2 are points on canvas left upper and right upper
        // *x - width of element
        // zoom position is upper RIGHT corner, so it used to draw with negative X

        // if selected interval smaller than zoom width, draw it on the right outer side, otherwise, right inner
        val zoomPosAdj1 = if (v2.x - v1.x < x) v2 + Geometry.V(x, 0) else v2

        // if it overlapped with zoomBack, push it down by zoomBack height
        val zoomPosAdj2 = if (zoomPosAdj1.x > thatCtx.getState.p0.x + bx + x) zoomPosAdj1 else zoomPosAdj1 + Geometry.V(0, y)
        val zoomPos = zoomPosAdj2 / cr

        Variables.zoomPos.update(s"translate(${zoomPos.x}, ${zoomPos.y})")
        Variables.zoomVisibility.update("visible")
        val fill = if (ifZoomPossible) Selecting.Common.green else Selecting.Common.red
        Variables.zoomFill.update(fill)

        // sort
        val sortPosAdj1 = if (v2.x - v1.x < 2 * x) v1 else v1 + Geometry.V(x, 0)
        val sortPosAdj2 = if (sortPosAdj1.x > thatCtx.getState.p0.x + bx + x) sortPosAdj1 else sortPosAdj1 + Geometry.V(0, y)
        val sortPosAdj3 = if (sortPosAdj2.x > thatCtx.getState.p0.x + x) sortPosAdj2 else v1 + Geometry.V(x, y)
        val sortPosAdj4 = if (sortPosAdj3.x < zoomPos.x * cr - x) sortPosAdj3 else sortPosAdj3 + Geometry.V(0, y)
        val sortPos = sortPosAdj4 / cr

        Variables.sortPos.update(s"translate(${sortPos.x}, ${sortPos.y})")
        Variables.sortVisibility.update("visible")
      case None ⇒
        Variables.zoomVisibility.update("hidden")
        Variables.sortVisibility.update("hidden")

    }
  }

  private object Variables {
    val R: SignalSource[Double] = Signal.source[Double]()
    val X: SignalSource[Double] = Signal.source[Double]()
    val BX: SignalSource[Double] = Signal.source[Double]()
    val Y: SignalSource[Double] = Signal.source[Double]()
    val ZTX: SignalSource[Double] = Signal.source[Double]()
    val BTX: SignalSource[Double] = Signal.source[Double]()
    val STX: SignalSource[Double] = Signal.source[Double]()
    val JTX: SignalSource[Double] = Signal.source[Double]()
    val TY: SignalSource[Double] = Signal.source[Double]()
    val F: SignalSource[Double] = Signal.source[Double]()

    val P0X: SignalSource[Double] = Signal.source[Double]()
    val P1Y: SignalSource[Double] = Signal.source[Double]()

    val zoomVisibility: SignalSource[String] = Signal.source[String]("hidden")
    val zoomFill: SignalSource[String] = Signal.source[String]()
    val zoomPos: SignalSource[String] = Signal.source[String]()

    val sortVisibility: SignalSource[String] = Signal.source[String]("hidden")
    val sortPos: SignalSource[String] = Signal.source[String]()

    val backHistory: SignalSource[Int] = Signal.source[Int](-1)

    val jumpsTopX: SignalSource[Map[Int, Double]] = Signal.source[Map[Int, Double]]()
    val topY: SignalSource[Double] = Signal.source[Double]()
    val realX: SignalSource[Double] = Signal.source[Double]()
    val realOrHist: SignalSource[Boolean] = Signal.source[Boolean](true)

    val gridX: SignalSource[Double] = Signal.source[Double]()
    val gridCoord: SignalSource[Boolean] = Signal.source[Boolean](false)

    val chartX: SignalSource[Double] = Signal.source[Double]()
    val chartFlag: SignalSource[Int] = Signal.source[Int](2)

    val zeroX: SignalSource[Double] = Signal.source[Double]()
    val zeroFlag: SignalSource[Boolean] = Signal.source[Boolean](false)

    val aggsTopX: SignalSource[Map[Int, Double]] = Signal.source[Map[Int, Double]]()
    val aggsTopY: SignalSource[Double] = Signal.source[Double]()
    val aggsR: SignalSource[Double] = Signal.source[Double]()
    val aggsX: SignalSource[Double] = Signal.source[Double]()
    val aggsY: SignalSource[Double] = Signal.source[Double]()
    val aggsF: SignalSource[Double] = Signal.source[Double]()
    val aggsTX: SignalSource[Double] = Signal.source[Double]()
    val aggsTY: SignalSource[Double] = Signal.source[Double]()
    val aggsIdx: SignalSource[Int] = Signal.source[Int](0)
  }

  def initSvg(): Unit = {

    import Signal.Implicits._
    import Tools._
    import Variables._
    import scalatags.JsDom.all._
    import scalatags.JsDom.svgTags
    import scalatags.JsDom.svgAttrs

    val selectZoom = svgTags.g(
      cursor.pointer,
      svgAttrs.visibility := zoomVisibility,
      svgAttrs.transform := zoomPos,
      svgTags.rect(
        svgAttrs.fill := zoomFill,
        svgAttrs.x := X.map(-_),
        svgAttrs.y := 0,
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.fontFamily := "'Font Awesome 5 Free'",
        svgAttrs.fontWeight := 900,
        svgAttrs.fontSize := F,
        svgAttrs.x := ZTX,
        svgAttrs.y := TY,
        Selecting.Common.lens),
      onclick := { () ⇒
        {
          if (ifZoomPossible) {
            zoom()
            reset()
            drawThrottle.immediate()
          }
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val selectOrder = svgTags.g(
      cursor.pointer,
      svgAttrs.visibility := sortVisibility,
      svgAttrs.transform := sortPos,
      svgTags.rect(
        svgAttrs.fill := Selecting.Common.green,
        svgAttrs.x := X.map(-_),
        svgAttrs.y := 0,
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.fontFamily := "'Font Awesome 5 Free'",
        svgAttrs.fontWeight := 900,
        svgAttrs.fontSize := F,
        svgAttrs.x := STX,
        svgAttrs.y := TY,
        Selecting.Common.sort),
      onclick := onSort,
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val selectBackSign = svgTags.tspan(
      svgAttrs.fontFamily := "'Font Awesome 5 Free'",
      svgAttrs.fontWeight := 900).render
    val selectBackNum = svgTags.tspan().render

    backHistory.listen { value ⇒
      val sign = if (value > 0) Selecting.Common.arrow else Selecting.Common.times
      clearChildren(selectBackSign)
      sign.applyTo(selectBackSign)

      val num = if (value > 9) " (…)" else if (value > 1) s" ($value)" else ""
      clearChildren(selectBackNum)
      num.applyTo(selectBackNum)
    }

    val selectBack = svgTags.svg(
      cursor.pointer,
      svgAttrs.visibility := backHistory.map(value ⇒ if (value >= 0) "visible" else "hidden"),
      svgAttrs.x := P0X,
      svgAttrs.y := P1Y,
      svgTags.rect(
        svgAttrs.fill := Selecting.Common.green,
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := BX,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.textAnchor := "middle",
        svgAttrs.fontSize := F,
        svgAttrs.x := BTX,
        svgAttrs.y := TY,
        selectBackSign,
        selectBackNum),
      onclick := { () ⇒
        {
          zoomBack()
          drawThrottle.immediate()
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val jumps = Selecting.jumpIntervals.map {
      case (idx, jumpName, jumpF) ⇒
        svgTags.svg(
          cursor.pointer,
          svgAttrs.x := jumpsTopX.map(m ⇒ m(idx)),
          svgAttrs.y := topY,
          svgTags.rect(
            svgAttrs.fill := Selecting.Common.blue,
            svgAttrs.rx := R,
            svgAttrs.ry := R,
            svgAttrs.width := X,
            svgAttrs.height := Y),
          svgTags.text(
            svgAttrs.textAnchor := "middle",
            svgAttrs.fontSize := F,
            svgAttrs.x := JTX,
            svgAttrs.y := TY,
            jumpName),
          onclick := { () ⇒
            {
              jump(jumpF)
              drawThrottle.immediate()
            }
          },
          onmouseover := { () ⇒ _isOnZoom = true },
          onmouseout := { () ⇒ _isOnZoom = false }).render
    }

    val realOrHistName = svgTags.tspan().render
    realOrHist.listen { b ⇒
      val sign = if (b) "R" else "H"
      clearChildren(realOrHistName)
      sign.applyTo(realOrHistName)
      if (b) {
        thatCtx.transform.setCoordinates(Geometry.CoordinatesUniverseRealTime)
        thatCtx.transform.setKeepRight(b = true)
      } else {
        thatCtx.transform.setCoordinates(Geometry.CoordinatesUniverse)
      }
      // use asap instead of immediate bcas of initialization order
      drawThrottle.later()
    }

    val realTimeHist = svgTags.svg(
      cursor.pointer,
      svgAttrs.x := realX,
      svgAttrs.y := topY,
      svgTags.rect(
        svgAttrs.fill := realOrHist.map(b ⇒ if (b) Selecting.Common.yellow else Selecting.Common.blue),
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.textAnchor := "middle",
        svgAttrs.fontSize := F,
        svgAttrs.x := JTX,
        svgAttrs.y := TY,
        realOrHistName),
      onclick := { () ⇒
        {
          val newVal = !realOrHist.value.getOrElse(false)
          realOrHist.update(newVal)
          drawThrottle.immediate()
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val gridSwitchName = svgTags.tspan().render
    gridCoord.listen { b ⇒
      val name = if (b) "GR" else "GH"
      clearChildren(gridSwitchName)
      name.applyTo(gridSwitchName)
      if (b) {
        thatCtx.transform.setGridCoordinates(Geometry.CoordinatesUniverseRealTime)
      } else {
        thatCtx.transform.setGridCoordinates(Geometry.CoordinatesUniverse)
      }
      // use asap instead of immediate bcas of initialization order
      drawThrottle.later()
    }

    val gridSwitch = svgTags.svg(
      cursor.pointer,
      svgAttrs.x := gridX,
      svgAttrs.y := topY,
      svgTags.rect(
        svgAttrs.fill := gridCoord.map(b ⇒ if (b) Selecting.Common.yellow else Selecting.Common.blue),
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.textAnchor := "middle",
        svgAttrs.fontSize := F,
        svgAttrs.x := JTX,
        svgAttrs.y := TY,
        gridSwitchName),
      onclick := { () ⇒
        {
          val newVal = !gridCoord.value.getOrElse(false)
          gridCoord.update(newVal)
          drawThrottle.immediate()
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val chartSwitchName = svgTags.tspan().render
    chartFlag.listen { i ⇒
      clearChildren(chartSwitchName)
      val chart = Charts.flagToChart(i)
      chart.name.applyTo(chartSwitchName)
      graphControl.setChart(chart)
      drawThrottle.later()
    }
    val chartSwitch = svgTags.svg(
      cursor.pointer,
      svgAttrs.x := chartX,
      svgAttrs.y := topY,
      svgTags.rect(
        svgAttrs.fill := chartFlag.map(i ⇒ Charts.flagToChart(i).color),
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.textAnchor := "middle",
        svgAttrs.fontSize := F,
        svgAttrs.x := JTX,
        svgAttrs.y := TY,
        chartSwitchName),
      onclick := { () ⇒
        {
          val newVal = (chartFlag.value.getOrElse(2) + 1) % Charts.flagToChart.size
          chartFlag.update(newVal)
          drawThrottle.immediate()
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    val zeroSwitchName = svgTags.tspan().render
    zeroFlag.listen { i ⇒
      clearChildren(zeroSwitchName)
      graphControl.setFromZero(i)
      val name = if (i) "Z" else "R"
      name.applyTo(zeroSwitchName)
      drawThrottle.later()
    }
    val zeroSwitch = svgTags.svg(
      cursor.pointer,
      svgAttrs.x := zeroX,
      svgAttrs.y := topY,
      svgTags.rect(
        svgAttrs.fill := chartFlag.map(i ⇒ Charts.flagToChart(i).color),
        svgAttrs.rx := R,
        svgAttrs.ry := R,
        svgAttrs.width := X,
        svgAttrs.height := Y),
      svgTags.text(
        svgAttrs.textAnchor := "middle",
        svgAttrs.fontSize := F,
        svgAttrs.x := JTX,
        svgAttrs.y := TY,
        zeroSwitchName),
      onclick := { () ⇒
        {
          zeroFlag.update(!zeroFlag.value.getOrElse(false))
          drawThrottle.immediate()
        }
      },
      onmouseover := { () ⇒ _isOnZoom = true },
      onmouseout := { () ⇒ _isOnZoom = false }).render

    svgLayer.appendChild(selectZoom)
    svgLayer.appendChild(selectOrder)
    svgLayer.appendChild(selectBack)
    jumps.foreach(svgLayer.appendChild)
    svgLayer.appendChild(realTimeHist)
    svgLayer.appendChild(gridSwitch)
    svgLayer.appendChild(chartSwitch)
    svgLayer.appendChild(zeroSwitch)
  }

  def initAggs(): Unit = {
    refreshSvg()

    import Signal.Implicits._
    import Variables._
    import scalatags.JsDom.all._
    import scalatags.JsDom.svgTags
    import scalatags.JsDom.svgAttrs

    selectingState.aggs.foreach(svgLayer.removeChild)

    aggsTopX.resetListeners()
    aggsTopY.resetListeners()
    aggsR.resetListeners()
    aggsX.resetListeners()
    aggsY.resetListeners()
    aggsF.resetListeners()
    aggsTX.resetListeners()
    aggsTY.resetListeners()
    aggsIdx.resetListeners()
    aggsIdx.update(0)

    val normalizedTransform = (y: GraphState) ⇒ (x: Double) ⇒ x / y.drawScale

    selectingState.aggs = graphControl.state.metric.toList.flatMap { mconf ⇒
      var adjAggs = mconf.aggregations.map(agg ⇒ (agg, 0))
      if (mconf.aggregations.contains(Aggregations.Sum)) {
        adjAggs = adjAggs :+ Aggregations.Sum -> 1
      }
      adjAggs.zipWithIndex.map {
        case ((agg, flg), idx) ⇒
          val name = if (flg == 0) agg.displayName else "norm"
          svgTags.svg(
            cursor.pointer,
            svgAttrs.x := aggsTopX.map(m ⇒ m(idx)),
            svgAttrs.y := aggsTopY,
            svgTags.rect(
              svgAttrs.fill := aggsIdx.map(currIdx ⇒ if (currIdx == idx) Selecting.Common.yellow else Selecting.Common.blue),
              svgAttrs.rx := aggsR,
              svgAttrs.ry := aggsR,
              svgAttrs.width := aggsX,
              svgAttrs.height := aggsY),
            svgTags.text(
              svgAttrs.textAnchor := "middle",
              svgAttrs.fontSize := aggsF,
              svgAttrs.x := aggsTX,
              svgAttrs.y := aggsTY,
              name),
            onclick := { () ⇒
              {
                aggsIdx.update(idx)
                if (flg == 0) {
                  graphControl.setAggregation(agg)
                } else {
                  graphControl.setAggregation(agg, normalizedTransform)
                }
                drawThrottle.immediate()
              }
            },
            onmouseover := { () ⇒ _isOnZoom = true },
            onmouseout := { () ⇒ _isOnZoom = false }).render
      }
    }

    selectingState.aggs.foreach(svgLayer.appendChild)
  }

  initSvg()

}
