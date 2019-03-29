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