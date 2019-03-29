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

package plutarch.server.data.objects

import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class ObjectsTest extends FunSuite with Matchers {
  test("emptyObjects") {
    val objects = Objects.create("empty")
    objects.getDe(Long.MinValue, Long.MaxValue).map(_.name) shouldBe Seq("total")
  }

  test("various") {
    val objects = Objects.create("various")

    objects.check(0, "obj1")
    objects.check(1000000, "obj1")
    objects.check(1000000, "obj2")
    objects.check(2000000, "obj1")
    objects.check(2000000, "obj2")
    objects.check(3000000, "obj1")
    objects.check(3000000, "obj2")
    objects.check(3000000, "obj3")
    objects.check(4000000, "obj2")
    objects.check(4000000, "obj3")
    objects.check(5000000, "obj2")
    objects.check(5000000, "obj3")
    objects.check(6000000, "obj3")
    objects.check(6000000, "obj4")

    objects.getDe(0, 6000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj1", "obj2", "obj3", "obj4")
    objects.getDe(6000000, 6000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj3", "obj4")
    objects.getDe(5000000, 6000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj2", "obj3", "obj4")
    objects.getDe(3000000, 4000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj1", "obj2", "obj3")
    objects.getDe(-1000000, 0).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj1")
    objects.getDe(6000000, 10000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj3", "obj4")
    objects.getDe(7000000, 10000000).map(_.name).filterNot(_ == "total").toSet shouldBe empty

    objects.check(100000000, "obj1")
    objects.getDe(7000000, 10000000).map(_.name).filterNot(_ == "total").toSet shouldBe Set("obj1")
  }

  test("many objects") {
    val objects = Objects.create("many")

    Random.setSeed(0)
    val objs = Random.shuffle(1.to(10000).map(i ⇒ s"obj$i"))

    objs.foreach(name ⇒ objects.check(0, name))
    objs.sorted.foreach(name ⇒ objects.check(1000, name))

    objects.getDe(200, 300).map(_.name).filterNot(_ == "total").toSet should contain allElementsOf objs.toSet

    objects.getDe(Long.MinValue, -1 - Objects.latency).map(_.name).filterNot(_ == "total").toSet shouldBe empty
    objects.getDe(1001 + Objects.latency, Long.MaxValue).map(_.name).filterNot(_ == "total").toSet shouldBe empty
  }

  test("real simultions") {
    val objects = Objects.create("real")

    val t0 = 0
    val t1 = 1000
    val maxtime = 100
    val count = 10000

    Random.setSeed(0)
    val objs = for (i ← 1 to count) yield {
      val objName = s"obj$i"
      val objFirst = t0 + Random.nextInt(t1 - t0)
      val objLast = objFirst + Random.nextInt(maxtime)
      (objFirst, objLast, objName)
    }

    for (t ← t0 to t1) {
      val thisObjs = objs.filter(obj ⇒ (obj._1 <= t && t <= obj._2) && (obj._1 == t || obj._2 == t || Random.nextDouble < 0.6))
      for (obj ← thisObjs) {
        objects.check(t, obj._3)
      }
    }

    val left = (t0 + t1) / 2
    val right = left + maxtime

    val expected = objs.filter(obj ⇒ obj._1 <= right && obj._2 >= left).map(_._3).toSet

    objects.getDe(left, right).map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expected
  }
}
