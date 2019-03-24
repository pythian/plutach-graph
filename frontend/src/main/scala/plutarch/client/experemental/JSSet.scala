package plutarch.client.experemental

import scala.scalajs.js
import js.annotation._

@js.native
@JSGlobal("Set")
class JSSet[A] extends js.Object {
  def this(iterable: js.Iterable[A]) = this()
  def size: Int = js.native
  def clear(): Unit = js.native
  def has(value: A): Boolean = js.native
  def add(value: A): this.type = js.native
  def delete(value: A): Boolean = js.native
  def keys(): js.Iterator[A] = js.native
  def forEach(callback: js.Function1[A, Unit]): Unit = js.native
}

object JSSet {
  def empty[A]: JSSet[A] = new JSSet[A]
}