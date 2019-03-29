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

import plutarch.client.graph.Geometry._
import plutarch.shared.data.MetricDomain
import plutarch.client.experemental.DateTools._
import slogging.LazyLogging

import scala.concurrent.duration._
import scala.collection.immutable.{ TreeMap, TreeSet }
import scalajs.js.Date

object Grid {
  def apply(graphControlState: GraphControlState)(ctx: Context): Grid = new Grid(graphControlState)(ctx)
}

class Grid(graphControlState: GraphControlState)(implicit ctx: Context) extends LazyLogging {
  val minLeftTick: Int = 200

  def draw(): Unit = {
    drawCover()
    drawGrid()
  }

  private def drawGrid(): Unit = {

    val (getUnit, dataFomat, heading) = (for {
      metric ← graphControlState.metric
      aggregation ← graphControlState.aggregation
    } yield {
      (aggregation.getUnit(metric), aggregation.dataFormat(metric), metric.name)
    }).getOrElse((MetricDomain.DefaultUnitSelector.apply _, MetricDomain.DefaultDataFormat.apply _, "None"))

    val ref = ctx.getState.p0.y / 4
    val refsize = 2 * ref / 3

    val availY = ctx.getState.nsize.y - ctx.getState.p0.y - ctx.getState.p1.y

    // Y-based calculations, the same font is used to X
    val font = ref
    val maxValues = math.min(30, (availY / ref).toInt)
    val fontY = Font(px = font, align = "start")

    val (gminh1, gmaxh1) = ctx.getGh1
    val coord = ctx.getState.gridCoordinates
    beginPath(gminh1 - 0.5 * NV(ctx.getState.p0.prjX))
      .lineTo(gmaxh1.prjX(CoordinatesFrame))
      .lineTo(gmaxh1 + 0.5 * NV(ctx.getState.p1.prjY))
      .stroke()

    // Y-axle
    val unitY = getUnit(ctx.getState.gsize.y, maxValues)
    val centerY = gmaxh1.prjX(coord)
    val stepY = GV(0, unitY)
    val directionY = L(ctx.getState.nsize.x)
    val dashY = R(refsize)
    val fontShiftY = R(2 * refsize)
    val (yn1, yn2) = getns(ctx.getState.gmin.y, ctx.getState.gmax.y, unitY)
    val yps = (yn1 to yn2).map(i ⇒ (i, centerY + i * stepY))
    lines(yps, directionY, "rgba(0,0,0,0.3)")
    lines(yps, dashY, "black")
    yps.foreach(p ⇒ (p._2 + fontShiftY).text(dataFomat(p._1 * unitY), fontY))
    if (yn2 - yn1 < 5) {
      val subStepY = GV(0, unitY / 10)
      val subDashY = R(refsize / 2)
      val (syn1, syn2) = getns(gminh1.prjY(coord).as(coord).y, gmaxh1.prjY(coord).as(coord).y, unitY / 10)
      val syps = (syn1 to syn2).map(i ⇒ (i, centerY + i * subStepY))
      lines(syps, subDashY, "black")
    }

    // X-axle
    if (coord == CoordinatesUniverse) {
      newStyleTicks(font, refsize)
    } else {
      oldStyleXTicks(font, refsize)
    }

    // name of Y-axle and Metric
    //(gmaxh1 + U(ctx.getState.p1.y + 2) + R(3 * refsize)).text(yDomain.unitName, Font(px = font, align = "start", baseLine = "top", fillStyle = "rgba(30,30,155,0.8)", bold = true))
    (gmaxh1.prjY(CoordinatesFrame) + U(ctx.getState.p1.y) + R(ctx.getState.nsize.x / 2)).text(heading, Font(px = 2 * font, /*align = "center", */ baseLine = "top", fillStyle = "rgba(255,100,0,0.5)", bold = true))
  }

  private def drawCover(): Unit = {
    val V(p0x, p0y) = ctx.getState.p0
    val V(p1x, p1y) = ctx.getState.p1
    NP(-p0x - 1, 0).clearRect(NV(ctx.c.size.x + 2, -p0y - 1))
    NP(-p0x - 1, ctx.getState.nsize.y).clearRect(NV(ctx.c.size.x + 2, p1y + 1))
    NP(0, -p0y - 1).clearRect(NV(-p0x - 1, ctx.c.size.y + 2))
    NP(ctx.getState.nsize.x, -p0y - 1).clearRect(NV(p1x + 1, ctx.c.size.y + 2))
  }

  private val offset: Long = {
    val date = new Date
    date.getTimezoneOffset() * 60000l
  }

  private val offsetH0: H0 = GV(offset, 0)

  private def getns(min: Double, max: Double, unit: Double): (Long, Long) = (Math.ceil(min / unit).toLong, Math.floor(max / unit).toLong)

  private def lines(ps: Iterable[(Long, H1)], direction: H0, color: String): Unit = {
    ps.foldLeft(beginPath)((acc, p) ⇒ acc.moveTo(p._2).lineBy(direction)).stroke(color)
  }

  private def oldStyleXTicks(font: Double, refsize: Double): Unit = {
    val (gminh1, gmaxh1) = ctx.getGh1
    val coord = ctx.getState.gridCoordinates
    def ifRealTime[T](a: ⇒ T, b: ⇒ T): T = if (coord == CoordinatesUniverseRealTime) a else b
    val fontX = Font(px = font, baseLine = "top")
    val unitX = msIntervalToUnit(ctx.getState.gsize.x, ctx.getState.nsize.x / ifRealTime(font * 8, font * 16))
    val centrX0 = gminh1.prjY(coord)
    val centerX = if (coord == CoordinatesUniverse) centrX0 + offsetH0 else centrX0
    val offset0 = if (coord == CoordinatesUniverse) offset else 0l
    val stepX = GV(unitX, 0)
    val directionX = U(ctx.getState.nsize.y)
    val dashX = D(refsize)
    val fontShiftX = D(5 * refsize / 4)
    val (xn1, xn2) = getns(gminh1.as(coord).x - offset0, gmaxh1.as(coord).x - offset0, unitX)
    val xps = (xn1 to xn2).map(i ⇒ (i, centerX + i * stepX))
    lines(xps, directionX, "rgba(0,0,0,0.3)")
    lines(xps, dashX, "black")
    xps.foreach(p ⇒ (p._2 + fontShiftX).text(ifRealTime(
      (d: Double) ⇒ toTimeStr(d, unitX),
      (d: Double) ⇒ toDateStr(d, unitX))(p._1 * unitX + offset0), fontX))
    if (xn2 - xn1 < 5) {
      val subStepX = GV(unitX / 10, 0)
      val subDashX = D(refsize / 2)
      val (sxn1, sxn2) = getns(gminh1.prjX(coord).as(coord).x, gmaxh1.prjX(coord).as(coord).x, unitX / 10)
      val sxps = (sxn1 to sxn2).map(i ⇒ (i, centerX + i * subStepX))
      lines(sxps, subDashX, "black")
    }
  }

  private def newStyleTicks(font: Double, refsize: Double): Unit = {
    val directionX = U(ctx.getState.nsize.y)
    val dashX = D(refsize)
    val doubleDashx = 2 * dashX
    val smallDash = D(2 * refsize / 3)
    val fontShiftX = D(refsize / 2)
    val fontShift2LineX = D(font + refsize / 2)
    val fontX = Font(px = font, align = "left", baseLine = "top")
    val fontXB = Font(px = font, align = "left", baseLine = "top", bold = true)
    val (x, y) = (ctx.getState.gmin.x.toLong, ctx.getState.gmax.x.toLong)
    val (ticks, smallTicks) = DateTicks.getTicks(x, y, ctx.getState.nsize.x, font)
    if (ticks.color.isDefined && !(ticks.useSub && smallTicks.isDefined && smallTicks.get.color.isDefined)) {
      val get = ticks.color.get
      var prevP = GP(x, ctx.getState.gmin.y)
      var prevC = get(x)
      var textPos: Option[Double] = None
      for (tick ← ticks.ticks) {
        val tickP = GP(tick.x, ctx.getState.gmin.y)
        prevP.fillRect(tickP + dashX, prevC)
        tickP.lineByStroke(directionX, "rgba(0,0,0,0.3)")
        (tickP + dashX).lineByStroke(doubleDashx, "black")
        tick.name.foreach { name ⇒
          (tickP + dashX + fontShiftX).text(name.text, if (name.bold) fontXB else fontX)
          name.text2.foreach { text2 ⇒
            if (textPos.isEmpty) textPos = Some(tickP.vCanvas.x)
            (tickP + dashX + fontShift2LineX).text(text2, fontXB)
          }
        }
        prevP = tickP
        prevC = get(tick.x)
      }
      if (!textPos.exists(_ < ctx.getState.p0.x + minLeftTick) && ticks.getFullName.isDefined) {
        //val fullText = ticks.getFullName.get(x).text2.get
        ticks.getFullName.get(x).text2.foreach(fullText ⇒ (V0 + dashX + fontShift2LineX).text(fullText, fontXB))
      }
      prevP.fillRect(GP(y, ctx.getState.gmin.y) + dashX, prevC)
      smallTicks.foreach { ticks ⇒
        beginPath
        for (tick ← ticks.ticks) {
          val tickP = GP(tick.x, ctx.getState.gmin.y) + dashX
          tickP.lineByStroke(smallDash, "black")
        }
      }
    } else if (smallTicks.isDefined && smallTicks.get.color.isDefined) {
      val get = smallTicks.get.color.get
      var prevP = GP(x, ctx.getState.gmin.y)
      var prevC = get(x)
      var textPos: Option[Double] = None
      for (tick ← smallTicks.get.ticks) {
        val tickP = GP(tick.x, ctx.getState.gmin.y)
        prevP.fillRect(tickP + dashX, prevC)
        prevP = tickP
        prevC = get(tick.x)
        (tickP + dashX).lineByStroke(smallDash, "black")
      }
      prevP.fillRect(GP(y, ctx.getState.gmin.y) + dashX, prevC)
      beginPath
      for (tick ← ticks.ticks) {
        val tickP = GP(tick.x, ctx.getState.gmin.y) + dashX
        tickP.lineByStroke(directionX, "rgba(0,0,0,0.3)")
        tickP.lineByStroke(doubleDashx, "black")
        tick.name.foreach { name ⇒
          (tickP + fontShiftX).text(name.text, if (name.bold) fontXB else fontX)
          name.text2.foreach { text2 ⇒
            if (textPos.isEmpty) textPos = Some(tickP.vCanvas.x)
            (tickP + fontShift2LineX).text(text2, fontXB)
          }
        }
      }
      if (!textPos.exists(_ < ctx.getState.p0.x + minLeftTick) && ticks.getFullName.isDefined) {
        ticks.getFullName.get(x).text2.foreach(fullText ⇒ (V0 + dashX + fontShift2LineX).text(fullText, fontXB))
      }
    } else {
      // unsipported combo, don't draw anything :-) or fallback to old?
    }
  }

  object DateTicks {

    case class Name(text: String, text2: Option[String] = None, bold: Boolean = false)

    case class Tick(x: Long, name: Option[Name])

    case class Ticks(ticks: TraversableOnce[Tick], color: Option[Long ⇒ String] = None, useSub: Boolean, getFullName: Option[Long ⇒ Name])

    type NameF = (Long, Boolean) ⇒ Name

    private def name(text: String): Name = Name(text, None)
    private def name(text: String, text2: String): Name = Name(text, Some(text2))
    private def nameIf(cond: Boolean, text2: ⇒ String, text: String): Name = if (cond) name(text, text2) else name(text)

    private sealed trait Splitter {
      def split(x: Long, y: Long): Ticks
      def skipAsSub: Boolean
    }

    private case class RangeSplitter(tree: TreeSet[Long], color: Option[Long ⇒ String] = None, skipAsSub: Boolean = false, useSub: Boolean = false, nameF: Option[NameF] = None) extends Splitter {
      def split(x: Long, y: Long): Ticks = {
        val ticks = tree.rangeImpl(Some(x), Some(y + 1)).iterator.map(t ⇒ {
          Tick(t, nameF.map(f ⇒ f(t, false)))
        })
        Ticks(ticks, color, useSub, nameF.map(f ⇒ t ⇒ f(t, true)))
      }
    }

    private case class NextSplitterStep(reference: Long, step: Long, color: Option[Long ⇒ Long ⇒ String] = None, skipAsSub: Boolean = false, useSub: Boolean = false, nameF: Option[NameF] = None) extends Splitter {
      def split(x: Long, y: Long): Ticks = {
        val first = ((x - reference) / step + 1) * step + reference
        val ticksLongs: Iterator[Long] = new Iterator[Long] {
          private var current = first
          def hasNext: Boolean = current <= y
          def next(): Long = {
            val res = current
            current = current + step
            res
          }
        }
        val ticks = ticksLongs.map { t ⇒
          Tick(t, nameF.map(f ⇒ f(t, false)))
        }
        Ticks(ticks, color.map(f ⇒ f(step)), useSub, nameF.map(f ⇒ t ⇒ f(t, true)))
      }
    }

    private case class NextSplitterFunc(nextFunc: Long ⇒ Long, color: Option[Long ⇒ String] = None, skipAsSub: Boolean = false, useSub: Boolean = false, nameF: Option[NameF] = None) extends Splitter {
      def split(x: Long, y: Long): Ticks = {
        val ticksLongs: Iterator[Long] = new Iterator[Long] {
          private var current = nextFunc(x)
          def hasNext: Boolean = current <= y
          def next(): Long = {
            val res = current
            current = nextFunc(current)
            res
          }
        }
        val ticks = ticksLongs.map { t ⇒
          Tick(t, nameF.map(f ⇒ f(t, false)))
        }
        Ticks(ticks, color, useSub, nameF.map(f ⇒ t ⇒ f(t, true)))
      }
    }

    private def nextWeekAdj(x: Long): Long = {
      val nextMonday = ((x - LOCAL_MONDAY) / SEVEN_DAYS + 1) * SEVEN_DAYS + LOCAL_MONDAY
      val thisDOW = (x - LOCAL_MONDAY) / ONE_DAY % 7
      val thisYear = new Date(x).getFullYear()
      val nextYear = new Date(nextMonday).getFullYear()
      if (thisYear == nextYear && thisDOW == 0) {
        val nextNextYear = new Date(nextMonday + SEVEN_DAYS).getFullYear()
        if (nextNextYear == thisYear) {
          nextMonday
        } else {
          val firstJanNextNextYear = new Date(nextNextYear, 0, 1).getTime().toLong
          if (firstJanNextNextYear - nextMonday > 3 * ONE_DAY) {
            nextMonday
          } else {
            firstJanNextNextYear
          }
        }
      } else if (thisYear == nextYear && thisDOW != 0) {
        val firstJanThisYear = new Date(thisYear, 0, 1).getTime()
        if (firstJanThisYear == x) {
          if (nextMonday - firstJanThisYear > 3 * ONE_DAY) {
            nextMonday
          } else {
            nextMonday + SEVEN_DAYS
          }
        } else {
          nextMonday
        }
      } else {
        new Date(nextYear, 0, 1).getTime().toLong
      }
    }

    private def dayColor(step: Long)(x: Long): String = {
      var dayNo = ((x - LOCAL_MONDAY) / step) % 7
      if (dayNo < 0) dayNo += 7
      if (dayNo >= 5) "rgba(255, 0, 0, 0.6)" else "rgba(0, 0, 0, 0.2)"
    }

    private def chessOrder(step: Long)(x: Long): String = {
      val dayNo = (((x - LOCAL_MONDAY) / 86400000l) % 7).toInt
      val ord = ((x - LOCAL_MONDAY) / step) % 2
      val (col1, col2) = if (dayNo >= 5) ("rgba(255, 0, 0, 0.7)", "rgba(255, 0, 0, 0.4)") else ("rgba(0, 0, 0, 0.2)", "rgba(0, 0, 0, 0.4)")
      if (ord % 2 == 0) col1 else col2
    }

    private def name1Y(x: Long, forcedFull: Boolean): Name = {
      val year = new Date(x).getFullYear()
      Name(year.toString, None, bold = true)
    }

    private def name10Y(x: Long, forcedFull: Boolean): Name = {
      val year = 10 * new Date(x).getFullYear() / 10
      Name(year.toString, None, bold = true)
    }

    private def nameQ(x: Long, forcedFull: Boolean): Name = {
      val date = new Date(x)
      val year = date.getFullYear()
      val quarter = date.getMonth() / 3 + 1
      nameIf(quarter == 1 || forcedFull, s"$year", s"Q$quarter")
    }

    private val monthsN = IndexedSeq[String]("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    private def nameM(x: Long, forcedFull: Boolean): Name = {
      val date = new Date(x)
      val year = date.getFullYear()
      val month = date.getMonth()
      val mon = monthsN(month)
      nameIf(month == 0 || forcedFull, s"$year", mon)
    }

    private def nameD(x: Long, forcedFull: Boolean): Name = {
      val date = new Date(x)
      val year = date.getFullYear()
      val month = date.getMonth()
      val mon = monthsN(month)
      val day = date.getDate()
      nameIf((month == 0 && day == 1) || forcedFull, s"$year", s"$mon-$day")
    }

    private def nameHM(x: Long, forcedFull: Boolean): Name = {
      val date = new Date(x)
      val year = date.getFullYear()
      val month = date.getMonth()
      val mon = monthsN(month)
      val day = date.getDate()
      val hours = date.getHours()
      val minutes = date.getMinutes()
      nameIf((hours == 0 && minutes == 0) || forcedFull, s"$year-$mon-$day", s"${l2(hours)}:${l2(minutes)}")
    }

    private def nameHMS(x: Long, forcedFull: Boolean): Name = {
      val date = new Date(x)
      val year = date.getFullYear()
      val month = date.getMonth()
      val mon = monthsN(month)
      val day = date.getDate()
      val hours = date.getHours()
      val minutes = date.getMinutes()
      val seconds = date.getSeconds()
      nameIf((hours == 0 && minutes == 0 && seconds == 0) || forcedFull, s"$year-$mon-$day", s"${l2(hours)}:${l2(minutes)}:${l2(seconds)}")
    }

    private def monthChessColor(x: Long): String = if (new Date(x).getMonth() % 2 == 0) "rgba(0, 0, 0, 0.2)" else "rgba(0, 0, 0, 0.4)"

    private def yearChessColor(x: Long): String = if (new Date(x).getFullYear() / 10 % 2 == 0) "rgba(0, 0, 0, 0.2)" else "rgba(0, 0, 0, 0.4)"

    private val scales = TreeMap[Long, Splitter](
      (14600.days.toMillis, NextSplitterFunc(next10Year, Some(yearChessColor), nameF = Some(name10Y))),
      (1460.days.toMillis, NextSplitterFunc(nextYear, nameF = Some(name1Y))),
      (365.days.toMillis, NextSplitterFunc(nextQuarter, skipAsSub = true, nameF = Some(nameQ))),
      (90.days.toMillis, NextSplitterFunc(nextMonth, Some(monthChessColor), useSub = true, nameF = Some(nameM))),
      (14.days.toMillis, NextSplitterFunc(nextWeekAdj, skipAsSub = true, nameF = Some(nameD))),
      (5.days.toMillis, NextSplitterStep(TZ_OFFSET, 1.day.toMillis, Some(dayColor), nameF = Some(nameD))),
      (15.hours.toMillis, NextSplitterStep(TZ_OFFSET, 6.hours.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (5.hours.toMillis, NextSplitterStep(TZ_OFFSET, 1.hour.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (3.hours.toMillis, NextSplitterStep(TZ_OFFSET, 30.minutes.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (1.hour.toMillis, NextSplitterStep(TZ_OFFSET, 10.minutes.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (15.minutes.toMillis, NextSplitterStep(TZ_OFFSET, 5.minutes.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (5.minutes.toMillis, NextSplitterStep(TZ_OFFSET, 1.minute.toMillis, Some(chessOrder), nameF = Some(nameHM))),
      (3.minutes.toMillis, NextSplitterStep(TZ_OFFSET, 30.seconds.toMillis, Some(chessOrder), nameF = Some(nameHMS))),
      (1.minute.toMillis, NextSplitterStep(TZ_OFFSET, 10.seconds.toMillis, Some(chessOrder), nameF = Some(nameHMS))),
      (15.seconds.toMillis, NextSplitterStep(TZ_OFFSET, 5.seconds.toMillis, Some(chessOrder), nameF = Some(nameHMS))),
      (0l, NextSplitterStep(TZ_OFFSET, 1.seconds.toMillis, Some(chessOrder), nameF = Some(nameHMS))))(Ordering.by(-_))

    def getTicks(x: Long, y: Long, width: Double, font: Double): (Ticks, Option[Ticks]) = {
      val koeff: Double = (width / font) / (3678 / 19)
      val scalesFrom = scales.from(((y - x) / koeff).toLong)
      val thisSplit = scalesFrom.head._2.split(x, y)
      (thisSplit, scalesFrom.tail.find(!_._2.skipAsSub).map(_._2.split(x, y)))
    }
  }

}
