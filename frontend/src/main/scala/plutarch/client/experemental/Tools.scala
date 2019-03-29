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
