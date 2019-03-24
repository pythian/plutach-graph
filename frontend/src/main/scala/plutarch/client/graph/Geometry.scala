package plutarch.client.graph

import org.scalajs.dom
import org.scalajs.dom.{ MouseEvent, TouchList }
import scala.scalajs.js
import scala.collection.mutable.ListBuffer

// todo fix step related show
object Geometry {

  object tools {
    def toIntervalOrdered(x: Double, x0: Double, x1: Double): Double = if (x < x0) x0 else if (x > x1) x1 else x
    def inIntervalOrdered(x: Double, x0: Double, x1: Double): Boolean = x0 <= x && x <= x1
    def toInterval(x: Double, x0: Double, x1: Double): Double = if (x0 <= x1) toIntervalOrdered(x, x0, x1) else toIntervalOrdered(x, x1, x0)
    def inInterval(x: Double, x0: Double, x1: Double): Boolean = inIntervalOrdered(x, x0, x1) || inIntervalOrdered(x, x1, x0)
  }

  case class V(x: Double, y: Double) {
    def dot(that: V): Double = this.x * that.x + this.y * that.y
    def +(that: V): V = V(this.x + that.x, this.y + that.y)
    def -(that: V): V = V(this.x - that.x, this.y - that.y)
    def *(that: V): V = V(this.x * that.x, this.y * that.y)
    def /(that: V): V = V(this.x / that.x, this.y / that.y)
    def >(that: V): Boolean = this.x > that.x && this.y > that.y
    lazy val inv = V(1d / x, 1d / y)
    def map(f: Double ⇒ Double): V = V(f(this.x), f(this.y))
    def *(d: Double): V = V(this.x * d, this.y * d)
    def /(d: Double): V = V(this.x / d, this.y / d)
    lazy val prjX: V = V(this.x, 0)
    lazy val prjY: V = V(0, this.y)
    lazy val length: Double = Math.sqrt(x * x + y * y)
    lazy val norm: V = this / length
    def toRect(v1: V, v2: V): V = V(tools.toInterval(this.x, v1.x, v2.x), tools.toInterval(this.y, v1.y, v2.y))
    def inRect(v1: V, v2: V): Boolean = tools.inInterval(this.x, v1.x, v2.x) && tools.inInterval(this.y, v1.y, v2.y)
    def vect(that: V): Double = this.x * that.y - this.y * that.x
    def intersect(p: V, a: V, b: V): Option[V] = {
      val at = a - this
      val n = p - this
      val m = b - a
      val d = m.vect(n)
      if (d == 0) {
        if (m.vect(p - a) == 0) {
          val bt = b - this
          val t0 = if (n.x != 0) at.x / n.x else at.y / n.y
          val t1 = if (n.x != 0) bt.x / n.x else bt.y / n.y
          if (0 <= t0 && t1 <= t0) {
            Some(a)
          } else if (0 <= t1 && t0 <= t1) {
            Some(b)
          } else {
            None
          }
        } else {
          None
        }
      } else {
        val s = n.vect(at) / d
        val t = m.vect(at) / d
        if (t >= 0 && s >= 0 && s <= 1) {
          val res = this + n * t
          Some(res)
        } else {
          None
        }
      }
    }
    override def toString: String = s"""{"x": $x, "y": $y}"""
  }

  case class M(a11: Double, a12: Double, a21: Double, a22: Double) {
    lazy val t: M = M(a11, a21, a12, a22)
    def *(v: V): V = V(a11 * v.x + a12 * v.y, a21 * v.x + a22 * v.y)
    def *(d: Double): M = M(a11 * d, a21 * d, a12 * d, a22 * d)
    def /(d: Double): M = M(a11 / d, a21 / d, a12 / d, a22 / d)
    def *(that: M): M = M(
      this.a11 * that.a11 + this.a12 * that.a21,
      this.a11 * that.a12 + this.a12 * that.a22,
      this.a21 * that.a11 + this.a22 * that.a21,
      this.a21 * that.a12 + this.a22 * that.a22)
    def det: Double = a11 * a22 - a12 * a21
    def inv: M = M(a22, -a21, -a12, a11) / det
  }

  implicit class ExtendedDouble(val d: Double) extends AnyVal {
    def *(v: V): V = v * d
    def *[T <: H[T]](v: H[T])(implicit ctx: Context): T = v * d
  }

  implicit class ExtendedInt(val i: Int) extends AnyVal {
    def *(v: V): V = v * i
    def *[T <: H[T]](v: H[T])(implicit ctx: Context): T = v * i
  }

  /*
  * Coordinates:
  *   CoordinatesUniverseRealTime: x=0 at *now*
  *   CoordinatesUniverse: absolute coordinates X-time, Y-value
  *   CoordinatesFrame: projection to given frame canvas, offset p0 and p1
  *   CoordinatesCanvas: point coordinates for canvas
  *
  * */
  trait Coordinates { val ord: Int }
  object CoordinatesUniverseRealTime extends Coordinates { val ord = 3 }
  object CoordinatesUniverse extends Coordinates { val ord = 2 }
  object CoordinatesFrame extends Coordinates { val ord = 1 }
  object CoordinatesCanvas extends Coordinates { val ord = 0 }

  /*
  * Wrapper for html Canvas with operations proper scaling:
  */
  object Canv {
    private var canvId = 0
    private def getNextCanvName = {
      canvId += 1
      s"canvas$canvId"
    }
    private var bsr: Option[Double] = None
    private def backingStoreRatio: Double = bsr.getOrElse {
      val lookup = dom.document.getElementById("canvas1")
      if (lookup != null) {
        val cmd =
          s"""var canvas = document.getElementById("canvas1");
             #var ctx = canvas.getContext("2d");
             #ctx.webkitBackingStorePixelRatio ||
             #  ctx.mozBackingStorePixelRatio ||
             #  ctx.msBackingStorePixelRatio ||
             #  ctx.oBackingStorePixelRatio ||
             #  ctx.backingStorePixelRatio || 1;""".stripMargin('#')
        bsr = Some(scalajs.js.eval(cmd).asInstanceOf[Double])
        bsr.get
      } else {
        1d
      }
    }
    def getRatio: Double = dom.window.devicePixelRatio / backingStoreRatio
    def apply(canv: dom.html.Canvas) = new Canv(canv)
  }

  class Canv(val canv: dom.html.Canvas) {
    import Canv._
    val brush: dom.CanvasRenderingContext2D = canv.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val id: String = getNextCanvName
    canv.id = id
    private var _width = canv.width
    private var _height = canv.height
    private var _size = V(_width, _height)
    def set(width: Int, height: Int): Unit = {
      _width = (width * getRatio).toInt
      _height = (height * getRatio).toInt
      _size = V(_width, _height)
      canv.width = _width
      canv.height = _height
      canv.style.width = s"${width}px"
      canv.style.height = s"${height}px"
    }
    def size: V = _size
    def mouseEventToPos(e: MouseEvent): V = {
      val rect = canv.getBoundingClientRect()
      V(e.clientX - rect.left, e.clientY - rect.top) * Canv.getRatio
    }
    def touchListToCTouchList(lst: TouchList): List[(Double, V)] = {
      val rect = canv.getBoundingClientRect()
      val lb = ListBuffer.empty[(Double, V)]
      var idx = 0
      while (idx < lst.length) {
        val touch = lst(idx)
        val pos = V(touch.clientX - rect.left, touch.clientY - rect.top) * Canv.getRatio
        lb += ((touch.identifier, pos))
        idx += 1
      }
      lb.result()
    }
    def clear(): Unit = {
      brush.clearRect(0, 0, _width, _height)
    }
  }

  /*
  * Points with coordinates
  * */
  abstract class H[T <: H[T]](v: V, coordinates: Coordinates, w: Int)(implicit ctx: Context) {
    def create(v: V, coordinates: Coordinates): T
    private var ctxIncarnation = ctx.state.incarnation
    private val vInCoordinates = new Array[V](4)
    vInCoordinates(coordinates.ord) = v
    private def transform(from: Int, to: Int): V = {
      var res = vInCoordinates(to)
      if (res == null) {
        val to2 = if (from < to) to - 1 else to + 1
        res = ctx.tm(w)(to2, to)(transform(from, to2))
        vInCoordinates(to) = res
      }
      res
    }
    def asUniverse: V = as(CoordinatesUniverse)
    def as(coordinates2: Coordinates): V = {
      if (ctxIncarnation != ctx.state.incarnation) {
        if (coordinates.ord != 0) vInCoordinates(0) = null
        if (coordinates.ord != 1) vInCoordinates(1) = null
        if (coordinates.ord != 2) vInCoordinates(2) = null
        if (coordinates.ord != 3) vInCoordinates(3) = null
        ctxIncarnation = ctx.state.incarnation
      }
      transform(coordinates.ord, coordinates2.ord)
    }
    def to(coordinates2: Coordinates): T = create(this.as(coordinates2), coordinates2)

    def vCanvas: V = as(CoordinatesCanvas)
    def +(that: H0): T = create(this.v + that.as(this.coordinates), this.coordinates)
    def -(that: H1): H0 = H0(this.v - that.as(this.coordinates), this.coordinates)
    def -(that: H0): T = create(this.v - that.as(this.coordinates), this.coordinates)
    def +(that: H1): T = create(this.v + that.as(this.coordinates), this.coordinates)
    def *(d: Double): T = create(v * d, coordinates)
    def /(d: Double): T = create(v / d, coordinates)
    def map(f: Double ⇒ Double, coordinates2: Coordinates): T = create(as(coordinates2).map(f), coordinates2)
    def prjX(coordinates2: Coordinates): T = create(as(coordinates2).prjX, coordinates2)
    def prjY(coordinates2: Coordinates): T = create(as(coordinates2).prjY, coordinates2)
    def inRect(v1: T, v2: T): Boolean = v.inRect(v1.as(coordinates), v2.as(coordinates))

    override def toString: String = s"""{"class": "${this.getClass.getName.split('$').last}", "coordinates": ${coordinates.ord}, "v": $v}"""
  }

  object H1 {
    def apply(v: V, coordinates: Coordinates)(implicit ctx: Context): H1 = new H1(v, coordinates)
  }

  class H1(v: V, coordinates: Coordinates)(implicit ctx: Context) extends H[H1](v, coordinates, 1) {
    def create(v: V, coordinates: Coordinates): H1 = new H1(v, coordinates)
    def isOnFrame: Boolean = inRect(ctx.gminh1, ctx.gmaxh1)
    def toFrameOnCanvas: V = {
      val c0 = ctx.tm(1)((1, 0))(Geometry.V(0, 0))
      val c1 = c0 + Geometry.V(1, -1) * ctx.state.nsize
      this.vCanvas.toRect(c0, c1)
    }
    def moveTo(p: H1): H1 = {
      ctx.c.brush.moveTo(p.vCanvas.x, p.vCanvas.y)
      p
    }
    def lineTo(p: H1): H1 = {
      ctx.c.brush.lineTo(p.vCanvas.x, p.vCanvas.y)
      p
    }
    def moveBy(v: H0): H1 = moveTo(this + v)
    def lineBy(v: H0): H1 = lineTo(this + v)
    def stroke(strokeStyle: String = "black", lineWidth: Int = 1): Unit = {
      if (lineWidth != 1) ctx.c.brush.lineWidth = lineWidth
      ctx.c.brush.strokeStyle = strokeStyle
      ctx.c.brush.stroke()
      ctx.c.brush.closePath()
      if (lineWidth != 1) ctx.c.brush.lineWidth = 1
    }
    def fill(fillStyle: String = "black"): Unit = {
      ctx.c.brush.fillStyle = fillStyle
      ctx.c.brush.fill()
      ctx.c.brush.closePath()
    }
    def fillAndStroke(fillStyle: js.Any, strokeStyle: js.Any): Unit = {
      ctx.c.brush.fillStyle = fillStyle
      ctx.c.brush.strokeStyle = strokeStyle
      ctx.c.brush.closePath()
      ctx.c.brush.fill()
      ctx.c.brush.stroke()
    }
    def text(s: String, font: Font): Unit = {
      font(ctx).fillText(s, vCanvas.x, vCanvas.y)
    }
    def text(s: String, font: Font, maxWidth: Double): Unit = {
      font(ctx).fillText(s, vCanvas.x, vCanvas.y, maxWidth)
    }
    def angleText(angle: Double, s: String, font: Font): Unit = {
      ctx.c.brush.save()
      ctx.c.brush.translate(vCanvas.x, vCanvas.y)
      ctx.c.brush.rotate(angle)
      font(ctx).fillText(s, 0, 0)
      ctx.c.brush.restore()
    }
    def lineToStroke(p2: H1, strokeStyle: String): Unit = {
      ctx.c.brush.beginPath()
      ctx.c.brush.moveTo(this.vCanvas.x, this.vCanvas.y)
      ctx.c.brush.lineTo(p2.vCanvas.x, p2.vCanvas.y)
      ctx.c.brush.strokeStyle = strokeStyle
      ctx.c.brush.stroke()
      ctx.c.brush.closePath()
    }
    def lineByStroke(v: H0, strokeStyle: String): Unit = lineToStroke(this + v, strokeStyle)
    def fillRect(nsize: H0, fillStyle: String): Unit = {
      ctx.c.brush.fillStyle = fillStyle
      ctx.c.brush.fillRect(vCanvas.x, vCanvas.y, nsize.vCanvas.x, nsize.vCanvas.y)
    }
    def fillRect(v: H1, fillStyle: String): Unit = {
      ctx.c.brush.fillStyle = fillStyle
      val sz = (v - this).vCanvas
      ctx.c.brush.fillRect(vCanvas.x, vCanvas.y, sz.x, sz.y)
    }
    def fillRectAndStroke(v: H1, fillStyle: String, strokeStyle: String): Unit = {
      ctx.c.brush.fillStyle = fillStyle
      ctx.c.brush.strokeStyle = strokeStyle
      val sz = (v - this).vCanvas
      ctx.c.brush.fillRect(vCanvas.x, vCanvas.y, sz.x, sz.y)
      ctx.c.brush.strokeRect(vCanvas.x, vCanvas.y, sz.x, sz.y)
    }
    def strokeRect(nsize: H0, strokeStyle: String): Unit = {
      ctx.c.brush.strokeStyle = strokeStyle
      ctx.c.brush.strokeRect(vCanvas.x, vCanvas.y, nsize.vCanvas.x, nsize.vCanvas.y)
    }
    def clearRect(nsize: H0): Unit = {
      ctx.c.brush.clearRect(vCanvas.x, vCanvas.y, nsize.vCanvas.x, nsize.vCanvas.y)
    }
    def dashLineBy(v: H0, l: Double): Unit = {
      if (l == 0) {
        this.lineBy(v)
      } else {
        val cnt = (v.vCanvas.length / l / 2d).toInt
        val d = H0(l * v.vCanvas.norm, CoordinatesCanvas)
        var i = 1
        var currPath = beginPath(this)
        while (i <= cnt) {
          currPath = currPath.lineBy(d).moveBy(d)
          i += 1
        }
      }
    }
    def intersect(p: H1, a: H1, b: H1): Option[H1] =
      this.as(this.coordinates).intersect(p.as(this.coordinates), a.as(this.coordinates), b.as(this.coordinates))
        .map(v ⇒ create(v, this.coordinates))
  }

  object H0 {
    def apply(v: V, coordinates: Coordinates)(implicit ctx: Context): H0 = new H0(v, coordinates)
  }

  class H0(v: V, coordinates: Coordinates)(implicit ctx: Context) extends H[H0](v, coordinates, 0) {
    def create(v: V, coordinates: Coordinates): H0 = new H0(v, coordinates)
    def *(v2: V): H0 = create(v2 * v, coordinates)
  }

  /*
  * Context of coordinates
  * */
  trait ContextInitConf {
    def p0: V
    def p1: V
    def current: Double
    def gmin: V
    def gmax: V
  }

  trait ContextLimitsConf {
    def minSize: Double
    def minChangePixels: Int
    def xLimits(state: ContextState): V
  }

  class ContextState(init: ContextInitConf, c: Canv) {
    var incarnation: Long = 0
    var coordinates: Coordinates = CoordinatesUniverse
    var gridCoordinates: Coordinates = CoordinatesUniverse
    var canvSize: V = c.size
    var p0: V = init.p0
    var p1: V = init.p1
    var nsize: V = canvSize - p0 - p1
    var current: Double = init.current
    var adjCurrent: Double = init.current
    var step: Double = 1.0
    var gmin: V = init.gmin
    var gmax: V = init.gmax
    var gsize: V = gmax - gmin

    override def toString: String =
      s"""{"incarnation": $incarnation, "coordinates": ${coordinates.ord}, "gridCoordinates": ${gridCoordinates.ord}, "canvSize": "$canvSize", "p0": $p0, "p1": $p1, "nsize": $nsize, "adjCurrent": $adjCurrent, "current": $current, "gmin": $gmin, "gmax": $gmax, "gsize": $gsize}"""
  }

  class ContextTransformationState {
    var scaling = 1.07
    var keepRight = false
  }

  class Scrolling() {
    var state = 0
    var startX: Double = 0
    var startXCanv: Double = 0
  }

  class Context(val c: Canv)(init: ContextInitConf, limits: ContextLimitsConf) {
    context ⇒
    private[Geometry] implicit val ctx: Context = this

    private[Geometry] val state = new ContextState(init, c)
    private[Geometry] val transformationState = new ContextTransformationState()
    private[Geometry] val scrolling = new Scrolling()

    private[Geometry] var gminh1 = H1(init.gmin, CoordinatesUniverse)
    private[Geometry] var gmaxh1 = H1(init.gmax, CoordinatesUniverse)

    def getGh1: (H1, H1) = (gminh1, gmaxh1)

    def scale: Double = state.gsize.x / state.nsize.x
    def getState: ContextState = state

    val tm: Map[Int, Map[(Int, Int), V ⇒ V]] = Map(
      1 → Map(
        (3, 2) → ((v: V) ⇒ v + V(state.current, 0)),
        (2, 3) → ((v: V) ⇒ v - V(state.current, 0)),
        (2, 1) → ((v: V) ⇒ (v - state.gmin) * state.nsize / state.gsize),
        (1, 2) → ((v: V) ⇒ state.gmin + v * state.gsize / state.nsize),
        (1, 0) → ((v: V) ⇒ V(0.5, state.canvSize.y - 0.5) + (v + state.p0) * V(1, -1)),
        (0, 1) → ((v: V) ⇒ (v - V(0.5, state.canvSize.y - 0.5)) * V(1, -1) - state.p0)),
      0 → Map(
        (3, 2) → ((v: V) ⇒ v),
        (2, 3) → ((v: V) ⇒ v),
        (2, 1) → ((v: V) ⇒ v * state.nsize / state.gsize),
        (1, 2) → ((v: V) ⇒ v * state.gsize / state.nsize),
        (1, 0) → ((v: V) ⇒ v * V(1, -1)),
        (0, 1) → ((v: V) ⇒ v * V(1, -1))))

    object transform {

      // b - keepright, shift - is moveCurrent required
      def setKeepRight(b: Boolean, shift: Boolean = true): Unit = {
        transformationState.keepRight = b
        if (b && shift) moveCurrent()
      }
      def isKeepRight: Boolean = transformationState.keepRight

      private def nextIncarnation(): Unit = state.incarnation += 1

      def setCoordinates(coordinates: Coordinates): Unit = state.coordinates = coordinates
      def setGridCoordinates(coordinates: Coordinates): Unit = state.gridCoordinates = coordinates
      def setCanvSize(width: Int, height: Int): Unit = {
        c.set(width, height)
        state.canvSize = c.size
        state.nsize = state.canvSize - state.p0 - state.p1
        nextIncarnation()
      }
      def setP(p0: V, p1: V): Unit = {
        state.p0 = p0
        state.p1 = p1
        state.nsize = state.canvSize - p0 - p1
        nextIncarnation()
      }
      def setStep(step: Double): Unit = {
        setStepCurrent(step, state.current)
      }
      def setCurrent(current: Double): Unit = {
        setStepCurrent(state.step, current)
      }
      private def setStepCurrent(step: Double, current: Double): Unit = {
        val delta = current - state.current
        state.current = current
        state.step = step
        state.adjCurrent = (state.current / step).floor * step
        if (delta != 0) {
          def transform(v: V): V = v + V(delta, 0)
          checkDomain(transform(state.gmin), transform(state.gmax))
        }
      }
      def rescale(pos: H1, wheel: Double): Unit = {
        val sc = if (wheel > 0) transformationState.scaling else 1d / transformationState.scaling
        val g = (if (isKeepRight) V(state.gmax.x, 0) else pos.asUniverse.prjX).toRect(state.gmin, state.gmax)
        def transform(v: V): V = g + (v - g) * V(sc, 1)
        if (sc > 1 || state.gsize.x * sc > limits.minSize) {
          checkDomain(transform(state.gmin), transform(state.gmax))
        }
      }
      def touchZoom(cp1: V, cp2: V, cr1: V, cr2: V): Unit = {
        val r1 = CP(cr1).asUniverse
        val r2 = CP(cr2).asUniverse
        if (r1.x != r2.x) {
          val p1 = CP(cp1).asUniverse
          val p2 = CP(cp2).asUniverse
          val k = (p1.x - p2.x) / (r1.x - r2.x)
          val b = p1.x - k * r1.x
          def transform(v: V): V = V(k, 1.0) * v + V(b, 0.0)
          if ((state.gmax.x - state.gmin.x) * k > limits.minSize) {
            checkDomain(transform(state.gmin), transform(state.gmax))
          }
        }
      }
      def setY(y1: Double, y2: Double): Unit = checkDomain(V(state.gmin.x, y1), V(state.gmax.x, y2))
      def setX(x1: Double, x2: Double): Unit = {
        checkDomain(V(x1, state.gmin.y), V(x2, state.gmax.y))
      }
      def moveCurrent(): Unit = {
        val g = V(state.adjCurrent - state.gmax.x, 0)
        checkDomain(state.gmin + g, state.gmax + g)
      }
      def scrollingState: Int = scrolling.state
      def scrollingStart(pos: H1): Unit = {
        scrolling.startX = pos.asUniverse.x
        scrolling.startXCanv = pos.vCanvas.x
        scrolling.state = 1
      }
      def getScrollingState: Int = scrolling.state
      def scrollingMove(pos: H1): Unit = if (Math.abs(pos.vCanvas.x - scrolling.startXCanv) > limits.minChangePixels) {
        val delta = V(scrolling.startX - pos.asUniverse.x, 0)
        checkDomain(state.gmin + delta, state.gmax + delta)
        scrolling.state = 2
      }
      def scrollingStop(): Unit = {
        scrolling.state = 0
      }

      private def checkDomain(newGmin: V, newGmax: V): Unit = {
        var (cgmin, cgmax) = (newGmin, newGmax)
        val V(gminminx, gmaxmaxx) = limits.xLimits(state)
        if (cgmax.x > gmaxmaxx) {
          val leftShift = cgmax.prjX - V(gmaxmaxx, 0)
          cgmin = cgmin - leftShift
          cgmax = cgmax - leftShift
          if (cgmin.x < gminminx) {
            cgmin = V(gminminx, cgmin.y)
          }
        } else if (cgmin.x < gminminx) {
          val rightShift = cgmin.prjX - V(gminminx, 0)
          cgmin = cgmin - rightShift
          cgmax = cgmax - rightShift
          if (cgmax.x > gmaxmaxx) {
            cgmax = V(gmaxmaxx, cgmax.y)
          }
        }
        state.gmin = cgmin
        state.gmax = cgmax
        state.gsize = cgmax - cgmin
        gminh1 = GP(cgmin)
        gmaxh1 = GP(cgmax)
        nextIncarnation()
      }
    }
  }

  def beginPath(p: H1)(implicit ctx: Context): H1 = {
    ctx.c.brush.beginPath()
    p.moveTo(p)
  }
  def beginPath(implicit ctx: Context): H1 = {
    ctx.c.brush.beginPath()
    V0
  }
  def lineArrow(p1: H1, p2: H1, l: Double = 10, a: Double = Math.PI / 16, strokeStyle: String = "black")(implicit ctx: Context): Unit = {
    val c = Math.cos(a)
    val s = Math.sin(a)
    val m = M(c, s, -s, c)
    val v = l * (p1 - p2).vCanvas.norm
    ctx.c.brush.strokeStyle = strokeStyle
    beginPath(p1).lineTo(p2).lineBy(CV(m * v)).moveTo(p2).lineBy(CV(m.t * v)).stroke()
  }
  def fill(fillStyle: String)(implicit ctx: Context): Unit = {
    ctx.c.brush.fillStyle = fillStyle
    ctx.c.brush.fillRect(0, 0, ctx.c.canv.width, ctx.c.canv.height)
  }

  def OP(v: V)(implicit ctx: Context): H1 = H1(v, CoordinatesUniverseRealTime)
  def GP(v: V)(implicit ctx: Context): H1 = H1(v, CoordinatesUniverse)
  def NP(v: V)(implicit ctx: Context): H1 = H1(v, CoordinatesFrame)
  def CP(v: V)(implicit ctx: Context): H1 = H1(v, CoordinatesCanvas)

  def OV(v: V)(implicit ctx: Context): H0 = H0(v, CoordinatesUniverseRealTime)
  def GV(v: V)(implicit ctx: Context): H0 = H0(v, CoordinatesUniverse)
  def NV(v: V)(implicit ctx: Context): H0 = H0(v, CoordinatesFrame)
  def CV(v: V)(implicit ctx: Context): H0 = H0(v, CoordinatesCanvas)

  def OP(x: Double, y: Double)(implicit ctx: Context): H1 = H1(V(x, y), CoordinatesUniverseRealTime)
  def GP(x: Double, y: Double)(implicit ctx: Context): H1 = H1(V(x, y), CoordinatesUniverse)
  def NP(x: Double, y: Double)(implicit ctx: Context): H1 = H1(V(x, y), CoordinatesFrame)
  def CP(x: Double, y: Double)(implicit ctx: Context): H1 = H1(V(x, y), CoordinatesCanvas)

  def OV(x: Double, y: Double)(implicit ctx: Context): H0 = H0(V(x, y), CoordinatesUniverseRealTime)
  def GV(x: Double, y: Double)(implicit ctx: Context): H0 = H0(V(x, y), CoordinatesUniverse)
  def NV(x: Double, y: Double)(implicit ctx: Context): H0 = H0(V(x, y), CoordinatesFrame)
  def CV(x: Double, y: Double)(implicit ctx: Context): H0 = H0(V(x, y), CoordinatesCanvas)

  def V0(implicit ctx: Context): H1 = NP(0, 0)

  def D(n: Double)(implicit ctx: Context): H0 = NV(0, -n)
  def U(n: Double)(implicit ctx: Context): H0 = NV(0, n)
  def L(n: Double)(implicit ctx: Context): H0 = NV(-n, 0)
  def R(n: Double)(implicit ctx: Context): H0 = NV(n, 0)

  case class Font(px: Double = 15, align: String = "center", baseLine: String = "middle", font: String = "Verdana", fillStyle: String = "black", bold: Boolean = false) {
    def apply(ctx: Context): dom.CanvasRenderingContext2D = {
      import ctx._
      c.brush.font = s"${if (bold) "bold " else ""}${px}px $font"
      c.brush.textAlign = align
      c.brush.textBaseline = baseLine
      c.brush.fillStyle = fillStyle
      c.brush
    }
  }

  // direct optimization instead of for example
  //   val gp = GP(V(p.key, p.value))
  //   path.moveTo(gp)

  @inline
  def directBeginPath(implicit ctx: Context): Unit = {
    ctx.c.brush.beginPath()
  }

  @inline
  def getCoefficients(implicit ctx: Context): (Double, Double, Double, Double) =
    (ctx.state.nsize.x / ctx.state.gsize.x,
      -ctx.state.nsize.y / ctx.state.gsize.y,
      -ctx.state.gmin.x * ctx.state.nsize.x / ctx.state.gsize.x + 0.5 + ctx.state.p0.x,
      ctx.state.gmin.y * ctx.state.nsize.y / ctx.state.gsize.y + ctx.state.canvSize.y - 0.5 - ctx.state.p0.y)

  @inline
  def directStroke(strokeStyle: String = "black", lineWidth: Int = 1)(implicit ctx: Context): Unit = {
    if (lineWidth != 1) ctx.c.brush.lineWidth = lineWidth
    ctx.c.brush.strokeStyle = strokeStyle
    ctx.c.brush.stroke()
    ctx.c.brush.closePath()
    if (lineWidth != 1) ctx.c.brush.lineWidth = 1
  }

  @inline
  def directFillAndStroke(fillStyle: js.Any, strokeStyle: js.Any)(implicit ctx: Context): Unit = {
    ctx.c.brush.fillStyle = fillStyle
    ctx.c.brush.strokeStyle = strokeStyle
    ctx.c.brush.closePath()
    ctx.c.brush.fill()
    ctx.c.brush.stroke()
  }

}
