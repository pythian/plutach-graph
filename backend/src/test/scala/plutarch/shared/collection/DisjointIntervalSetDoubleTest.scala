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
import plutarch.shared.Protocol.Interval

import scala.util.Random

class DisjointIntervalSetDoubleTest extends FunSuite with Matchers {
  test("emptySet") {
    val intervals = DisjointIntervalSetDouble.empty

    val i0 = Interval(0, 100)
    intervals.diff(i0) shouldBe Seq(i0)
    intervals.intersect(i0) shouldBe empty

    intervals.add(0, 1)
    intervals.remove(0, 1)
    intervals.getLength shouldBe 0
  }

  test("set with 1 element") {
    val intervals = DisjointIntervalSetDouble.empty
    intervals.add(20, 50)

    val i0 = Interval(0, 100)
    intervals.diff(i0) shouldBe Seq(Interval(0, 20), Interval(50, 100))
    intervals.intersect(i0) shouldBe Seq(Interval(20, 50))

    val i1 = Interval(0, 30)
    intervals.diff(i1) shouldBe Seq(Interval(0, 20))
    intervals.intersect(i1) shouldBe Seq(Interval(20, 30))

    val i2 = Interval(30, 100)
    intervals.diff(i2) shouldBe Seq(Interval(50, 100))
    intervals.intersect(i2) shouldBe Seq(Interval(30, 50))

    intervals.add(1, 2)
    intervals.diff(i2)
    intervals.shrink(30)

    intervals.getLength shouldBe 30
  }

  test("merged to 1 big") {
    val intervals = DisjointIntervalSetDouble.empty
    Random.setSeed(0)
    val i0 = Random.shuffle((0 until 100).map(i ⇒ Interval(i, i + 1)))

    intervals.add(i0)

    intervals.getLength shouldBe 100

    intervals.diff(-1, 101) shouldBe Seq(Interval(-1, 0), Interval(100, 101))

    intervals.shrink(50)
    intervals.diff(0, 100).map(x ⇒ x.right - x.left).sum shouldBe 50

    intervals.clear()
    intervals.add(i0.take(30))
    val i1 = intervals.diff(0, 100)
    intervals.add(i1)
    intervals.intersect(-1, 101) shouldBe Seq(Interval(0, 100))

    intervals.clear()
    intervals.add(i0.take(60))
    intervals.remove(i0.take(10))
    val i2 = intervals.diff(0, 100)
    i2.map(x ⇒ x.right - x.left).sum shouldBe 50
  }

  test("refresh test") {
    val intervals = DisjointIntervalSetDouble.empty
    intervals.add(0, 10)
    for (i ← 2 to 100) {
      intervals.add(i * 10, i * 10 + 10)
      intervals.diff(-1, 1)
      intervals.diff(19, 20)
      if (i > 20) intervals.shrink(10)
    }
    intervals.shrink(10)
    intervals.getLength shouldBe 10
    intervals.intersect(Double.MinValue, Double.MaxValue) shouldBe Seq(Interval(0, 10))
  }

  test("bug #1") {
    val intervals = DisjointIntervalSetDouble.empty
    intervals.add(5, 10)
    intervals.diff(30, 40) shouldBe List(Interval(30, 40))
  }

}

