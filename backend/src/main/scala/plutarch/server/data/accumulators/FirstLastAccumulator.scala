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

package plutarch.server.data.accumulators

import plutarch.shared.data.Aggregations._

class FirstLastAccumulator extends Accumulator {
  private var firstT: Long = 0
  private var firstValue: Double = 0
  private var lastT: Long = 0
  private var lastValue: Double = 0
  private var empty = true
  def add(t: Long, value: Double): Unit = {
    if (empty) {
      firstT = t
      firstValue = value
      lastT = t
      lastValue = value
      empty = false
    } else {
      if (firstT > t) {
        firstT = t
        firstValue = value
      }
      if (lastT <= t) {
        lastT = t
        lastValue = value
      }
    }
  }
  //def getFirst: Option[Double] = if (empty) None else Some(firstValue)
  //def getLast: Option[Double] = if (empty) None else Some(lastValue)
  def getFirst: Double = firstValue
  def getLast: Double = lastValue
}

object FirstLastAccumulator extends AccumulatorCompanion {
  type A = FirstLastAccumulator
  def apply(): A = new FirstLastAccumulator()
  def getInstanceCreator(aggregations: Seq[Aggregation]): () ⇒ A = () ⇒ apply()
  val get: PartialFunction[Aggregation, A ⇒ Any] = {
    case First ⇒ _.getFirst
    case Last  ⇒ _.getLast
  }
}