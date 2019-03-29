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