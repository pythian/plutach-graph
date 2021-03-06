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

package plutarch.server.data.experemental

object Tools {
  @inline
  def timing[T](name: String)(action: ⇒ T): T = {
    val t0 = System.nanoTime()
    val res = action
    val t1 = System.nanoTime()
    println(s"$name executed in ${(t1 - t0) / 1e6} ms")
    res
  }
}
