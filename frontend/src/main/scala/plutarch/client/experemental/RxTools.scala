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

import org.scalajs.dom
import rx._
import scala.concurrent.duration._
import scala.annotation.tailrec
import scala.util.{ Failure, Success }
import scalatags.JsDom.all._
import org.scalajs.dom.Element
import scalatags.JsDom.TypedTag

object RxTools {

  // https://github.com/Voltir/framework.rx/blob/master/src/main/scala/framework/Framework.scala
  // see also ideas at https://github.com/rtimush/scalatags-rx/blob/master/src/main/scala/scalatags/rx/styles.scala
  implicit def RxFrag[T](n: Rx[T])(implicit f: T ⇒ Frag, ctx: Ctx.Owner): Frag = {

    @tailrec def clearChildren(node: org.scalajs.dom.Node): Unit = {
      if (node.firstChild != null) {
        node.removeChild(node.firstChild)
        clearChildren(node)
      }
    }

    def fSafe: Frag = n match {
      case r: Rx.Dynamic[T] ⇒ r.toTry match {
        case Success(v) ⇒ v.render
        case Failure(e) ⇒ scalatags.JsDom.all.span(e.getMessage, backgroundColor := "red").render
      }
      case v: Var[T] ⇒ v.now.render
    }

    var last = fSafe.render

    val container = scalatags.JsDom.all.span(cls := "_rx")(last).render

    n.triggerLater {
      val newLast = fSafe.render
      //Rx[Seq[T]] can generate multiple children per propagate, so use clearChildren instead of replaceChild
      clearChildren(container)
      container.appendChild(newLast)
      last = newLast
    }
    bindNode(container)
  }

  implicit def RxAttrValue[T: AttrValue](implicit ctx: Ctx.Owner) = new AttrValue[Rx.Dynamic[T]] {
    def apply(t: Element, a: Attr, r: Rx.Dynamic[T]): Unit = {
      r.trigger { implicitly[AttrValue[T]].apply(t, a, r.now) }
    }
  }

  implicit def RxStyleValue[T: StyleValue](implicit ctx: Ctx.Owner) = new StyleValue[Rx.Dynamic[T]] {
    def apply(t: Element, s: Style, r: Rx.Dynamic[T]): Unit = {
      r.trigger { implicitly[StyleValue[T]].apply(t, s, r.now) }
    }
  }

  implicit def RxStyleAValue(r: Rx.Dynamic[Option[scalacss.StyleA]])(implicit ctx: Ctx.Owner) = new Modifier {
    def applyTo(t: Element): Unit = {
      var prev: Option[scalacss.StyleA] = None
      r.foreach { cur ⇒
        prev.foreach(s ⇒ t.removeClass(s))
        cur.foreach(s ⇒ t.addClass(s))
        prev = cur
      }
    }
  }

  case class InnerTextFrag(v: String) extends Modifier {
    def applyTo(t: Element): Unit = t.textContent = v
  }

  implicit class RxRawFragBinder(n: Rx.Dynamic[RawFrag])(implicit ctx: Ctx.Owner) {
    def into(container: TypedTag[dom.Element]): Frag = {
      val renderedContainer: dom.Element = container.render
      n.now.applyTo(renderedContainer)
      n.triggerLater {
        renderedContainer.innerHTML = ""
        val newLast = n.now
        n.now.applyTo(renderedContainer)
      }
      bindNode(renderedContainer)
    }
  }

  // custom version for svg text
  implicit class RxFragBinder[T](n: Rx[T])(implicit f: T ⇒ Frag, ctx: Ctx.Owner) {
    def into(container: Frag): Frag = {
      val renderedContainer = container.render
      var last = n.now.render
      renderedContainer.appendChild(last)
      n.triggerLater {
        val newLast = n.now.render
        renderedContainer.replaceChild(newLast, last)
        last = newLast
      }
      bindNode(renderedContainer)
    }
  }

  // custom version for append children append/remove
  implicit class RxSeqFragBinder[T](n: Rx[Seq[T]])(implicit f: T ⇒ Frag, ctx: Ctx.Owner) {
    def into(container: Frag): Frag = {
      val renderedContainer = container.render
      var last: Seq[dom.Node] = Nil
      n.foreach { newseq ⇒
        last.foreach(renderedContainer.removeChild)
        last = newseq.map(_.render)
        last.foreach(renderedContainer.appendChild)
      }
      bindNode(renderedContainer)
    }
  }

  // custom version for svg text
  implicit class RxModifierBinder[T](n: Rx[T])(implicit f: T ⇒ Modifier, ctx: Ctx.Owner) {
    def into(container: dom.Element): Frag = {
      n.trigger {
        n.now.applyTo(container)
      }
      bindNode(container)
    }
  }

  // advanced Itefable
  implicit class AdvancedIterable[T](underlying: Iterable[T])(implicit ctx: Ctx.Owner) {
    import rx.async._
    import rx.async.Platform._
    def toRx(interval: Long) = {
      val iterator = underlying.iterator
      val timer = Timer(interval millis)
      timer.map { x ⇒
        val res = iterator.next()
        if (!iterator.hasNext) {
          timer.kill()
        }
        res
      }
    }
  }

  // advanced HTMLElement
  implicit class ExtendedElement[T <: Element](el: T) {
    import scalacss.StyleA

    def changeClass(from: StyleA, to: StyleA) = {
      removeClass(from)
      addClass(to)
    }

    def removeClass(clss: StyleA*) =
      for {
        cls ← clss
        clsNames ← cls.classNameIterator
      } el.classList.remove(clsNames.value)

    def addClass(clss: StyleA*) =
      for {
        cls ← clss
        clsNames ← cls.classNameIterator
      } el.classList.add(clsNames.value)
  }

}
