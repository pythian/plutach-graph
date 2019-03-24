package plutarch.client.graph

import Geometry._
import org.scalajs.dom.MouseEvent

object Cursor {
  def apply(c: Canv) = new Cursor(c)

  class State() {
    var onHold = false
    var pos: Option[V] = None
  }
}

class Cursor(c: Canv) {
  import Cursor._
  val state = new State()

  def getPos(implicit ctx: Context): Option[H1] = state.pos.map(Geometry.CP)

  def move(e: MouseEvent): Unit = moveTo(c.mouseEventToPos(e))

  def moveTo(pos: V): Unit = if (!state.onHold) {
    state.pos = Some(pos)
    c.clear()
    draw()
  }

  def moveAway(): Unit = if (!state.onHold) {
    state.pos = None
    c.clear()
  }

  def refresh(): Unit = {
    draw()
  }

  private def draw(): Unit = state.pos match {
    case Some(p) ⇒
      c.brush.fillStyle = if (state.onHold) "red" else "green"
      c.brush.fillRect(p.x - 1, 0, 2, c.size.y)
      if (state.onHold) {
        c.brush.beginPath()
        c.brush.fillStyle = "rgb(0,0,255)"
        c.brush.arc(p.x, p.y, 5, 0, 2 * Math.PI)
        c.brush.fill()
      }
    case None ⇒
  }

  def isOnHold: Boolean = state.onHold

  def toggleHold(): Unit = {
    state.onHold = !state.onHold
    c.clear()
    draw()
  }

}