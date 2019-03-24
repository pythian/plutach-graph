package plutarch.client.experemental

import org.scalajs.dom.Element
import scalatags.JsDom.all.{ Attr, AttrValue, Style, StyleValue }
import slogging.LazyLogging
import scala.language.higherKinds
/*
* This has to handle simple use case of scalarx which is used before.
* it's only about attribute bindings
* */

trait Signal[+T] {
  def value: Option[T]
  def listen(listener: T ⇒ Unit): Unit
  def map[U](fun: T ⇒ U): Signal[U] = {
    val res = Signal.source[U]()
    this.listen(t ⇒ res.update(fun(t)))
    res
  }
  def zip[U](that: Signal[U]): Signal[(T, U)] = {
    val res = Signal.source[(T, U)]()
    this.listen { t ⇒
      that.value.foreach(u ⇒ res.update((t, u)))
    }
    that.listen { u ⇒
      this.value.foreach(t ⇒ res.update((t, u)))
    }
    res
  }
}

trait SignalSource[T] extends Signal[T] {
  def update(value: T): Unit
  def resetListeners(): Unit
}

class SignalSourceImpl[T] extends SignalSource[T] {
  private var _last: Option[T] = None
  private var _listeners = List.empty[T ⇒ Unit]
  def update(value: T): Unit = {
    if (!_last.contains(value)) {
      _listeners.foreach(listener ⇒ listener(value))
      _last = Some(value)
    }
  }
  def listen(listener: T ⇒ Unit): Unit = {
    _listeners = listener :: _listeners
    _last.foreach(listener)
  }
  def value: Option[T] = _last
  def resetListeners(): Unit = _listeners = List.empty
}

object Signal {

  trait Context extends LazyLogging {
    def log(str: String): Unit = logger.info(str)
  }

  def source[T](): SignalSource[T] = new SignalSourceImpl[T]()

  def source[T](init: T): SignalSource[T] = {
    val res = new SignalSourceImpl[T]()
    res.update(init)
    res
  }

  object Implicits {

    implicit object Ctx extends Context

    implicit def SubjectAttrValue[T: AttrValue, F[U] <: Signal[U]](implicit ctx: Context): AttrValue[F[T]] = new AttrValue[F[T]] {
      def apply(t: Element, a: Attr, r: F[T]): Unit = {
        //ctx.log("SubjectAttrValue.listen")
        r.listen { value ⇒ implicitly[AttrValue[T]].apply(t, a, value) }
      }
    }

    implicit def SubjectStyleValue[T: StyleValue, F[U] <: Signal[U]](implicit ctx: Context): StyleValue[F[T]] = new StyleValue[F[T]] {
      def apply(t: Element, s: Style, r: F[T]): Unit = {
        //ctx.log("SubjectStyleValue.listen")
        r.listen { value ⇒ implicitly[StyleValue[T]].apply(t, s, value) }
      }
    }

  }

}