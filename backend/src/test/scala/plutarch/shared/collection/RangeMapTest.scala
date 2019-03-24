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
