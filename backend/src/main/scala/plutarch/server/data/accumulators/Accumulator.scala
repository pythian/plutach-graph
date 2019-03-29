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

import plutarch.shared.data.Aggregations.Aggregation

trait Accumulator {
  def add(t: Long, value: Double): Unit
}

trait AccumulatorCompanion {
  type A <: Accumulator
  def getInstanceCreator(aggregations: Seq[Aggregation]): () ⇒ A
  val get: PartialFunction[Aggregation, A ⇒ Any]
}

