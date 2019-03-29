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

package plutarch.shared.collection

import org.scalatest.{ FunSuite, Matchers }

class RangeMapTest extends FunSuite with Matchers {
  test("ByteOffset test") {
    val step = 10000
    val x0 = 93823
    val brm = new ByteRangeMap(step, 1000)
    for (key ← (x0 * step).to((x0 + 100) * step, step)) {
      brm.add(key, key)
    }
    brm.getOrElse(0, -33) shouldBe -33
    brm.getOrElse(Long.MaxValue, -33) shouldBe -33
    brm.getOrElse(Long.MinValue, -33) shouldBe -33
    val key = (x0 + 50) * step
    brm.getOrElse(key, -33) shouldBe key
  }
}
