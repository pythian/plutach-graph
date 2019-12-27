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

package plutarch.server.data.scale

import java.nio.ByteBuffer

import org.scalatest.{ FunSuite, Matchers }

class MultiByteBufferAggregationStoreTest extends FunSuite with Matchers {

  def padding(len: Int, c: Char) = {
    val bb = ByteBuffer.allocate(len)
    for (i ← 0 until len) {
      bb.put(c.asInstanceOf[Byte])
    }
    bb.rewind()
    bb
  }

  test("query empty sould empty") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    s.get(1000, 2000).value.get.get.remaining() shouldBe 0
  }

  test("put in empty 1") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    //val s = ByteBufferAggregationStore.create(1000, 100, 1000)
    s.add(1000, padding(900, '1'))
    val bb = s.get(1000, 2000).value.get.get
    bb.remaining() shouldBe 900
    bb.get() shouldBe '1'.asInstanceOf[Byte]
  }

  test("put empty 2") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    //val s = ByteBufferAggregationStore.create(1000, 100, 1000)
    s.add(1000, padding(1000, '1'))
    val bb = s.get(1000, 2000).value.get.get
    bb.remaining() shouldBe 1000
    bb.get() shouldBe '1'.asInstanceOf[Byte]
  }

  test("put empty 3") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    assertThrows[AssertionError] {
      s.add(1000, padding(1001, '1'))
    }
  }

  test("put 1") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    s.add(1000, padding(300, '1'))
    s.add(2000, padding(300, '2'))
    s.add(3000, padding(300, '3'))

    s.add(4000, padding(300, '4'))
    s.add(5000, padding(300, '5'))

    s.getPages.size() shouldBe 2

    val bb = s.getSync(2000, 8000)

    bb.remaining() shouldBe 1200

    bb.get(0) shouldBe '2'.asInstanceOf[Byte]
    bb.get(300) shouldBe '3'.asInstanceOf[Byte]
    bb.get(600) shouldBe '4'.asInstanceOf[Byte]
    bb.get(900) shouldBe '5'.asInstanceOf[Byte]
    bb.get(1199) shouldBe '5'.asInstanceOf[Byte]

  }

  test("put 2") {
    val s = MultiByteBufferAggregationStore.create(1000, 100, 1000)
    s.add(1000, padding(300, '1'))
    s.add(2000, padding(300, '2'))
    s.add(3000, padding(300, '3'))

    s.add(4000, padding(300, '4'))
    s.add(5000, padding(300, '5'))
    s.add(6000, padding(300, '6'))

    s.add(7000, padding(300, '7'))
    s.add(8000, padding(300, '8'))
    s.add(9000, padding(300, '9'))

    s.getPages.size() shouldBe 3

    val bb = s.getSync(2000, 7000)

    bb.remaining() shouldBe 1500

    bb.get(0) shouldBe '2'.asInstanceOf[Byte]
    bb.get(300) shouldBe '3'.asInstanceOf[Byte]
    bb.get(600) shouldBe '4'.asInstanceOf[Byte]
    bb.get(900) shouldBe '5'.asInstanceOf[Byte]
    bb.get(1200) shouldBe '6'.asInstanceOf[Byte]

  }

}
