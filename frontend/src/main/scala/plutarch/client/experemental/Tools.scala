package plutarch.client.experemental

import org.scalajs.dom.Element
import scalatags.JsDom.all.Modifier

import scala.annotation.tailrec
import scala.scalajs.js.timers.{ SetTimeoutHandle, clearTimeout, setTimeout }

object Tools {

  def now(): Long = System.currentTimeMillis()

  case class Throttle(delay: Int)(action: ⇒ Unit) {
    private var last = 0l
    private var handle: Option[SetTimeoutHandle] = None
    private def runAction(): Unit = {
      action
      last = now()
    }
    def asap(): Unit = {
      if (handle.isEmpty) {
        val left = last + delay - now()
        if (left <= 0) {
          runAction()
        } else {
          handle = Some(setTimeout(left) {
            runAction()
            handle = None
          })
        }
      }
    }
    def later(): Unit = {
      if (handle.isEmpty) {
        handle = Some(setTimeout(delay) {
          runAction()
          handle = None
        })
      }
    }
    def immediate(): Unit = {
      cancel()
      runAction()
    }
    def cancel(): Unit = if (handle.isDefined) {
      clearTimeout(handle.get)
      handle = None
    }
  }

  class InnerText(v: String) extends Modifier {
    def applyTo(t: Element): Unit = t.textContent = v
  }

  def innerText(v: String): InnerText = new InnerText(v)

  @tailrec def clearChildren(node: org.scalajs.dom.Node): Unit = {
    if (node.firstChild != null) {
      node.removeChild(node.firstChild)
      clearChildren(node)
    }
  }

  @inline
  def timing[T](name: String)(action: ⇒ T): T = {
    val t0 = System.nanoTime()
    val res = action
    val t1 = System.nanoTime()
    println(s"$name executed in ${(t1 - t0) / 1e6} ms")
    res
  }

}
