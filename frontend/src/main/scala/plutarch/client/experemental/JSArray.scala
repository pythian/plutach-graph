package plutarch.client.experemental

import scala.scalajs.js
import js.annotation._

@js.native
@JSGlobal("Array")
class JSArray[A] extends js.Object {
  def length: Int = js.native
  @JSBracketAccess
  def apply(index: Int): A = js.native
  @JSBracketAccess
  def update(index: Int, value: A): Unit = js.native
  @JSName(js.Symbol.iterator)
  def jsIterator(): js.Iterator[A] = js.native
  def push(items: A*): Int = js.native
  def forEach(callback: js.Function1[A, Unit]): Unit = js.native
  def sort(compareFn: js.Function2[A, A, Int]): JSArray[A] = js.native
}

object JSArray {
  def empty[A]: JSArray[A] = new JSArray[A]
}