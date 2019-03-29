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

trait CombinedAccumulator extends Accumulator {
  def add(t: Long, value: Double): Unit
  def get(aggregation: Aggregation): Any
  def getAll: IndexedSeq[Any]
}

trait CombinedAccumulatorCreator {
  def getAggregations: Seq[Aggregation]
  def create(): CombinedAccumulator
}

object CombinedAccumulator {
  def getInstanceCreator(aggregations: Seq[Aggregation]): CombinedAccumulatorCreator = {
    val aggToAccComp: Map[Aggregation, AccumulatorCompanion] = Accumulators.getMap(aggregations)

    val accComps: Map[AccumulatorCompanion, () ⇒ Accumulator] = aggToAccComp
      .values.toSet
      .map((accComp: AccumulatorCompanion) ⇒
        accComp -> accComp.getInstanceCreator(aggregations))
      .toMap

    val gets: Map[Aggregation, Accumulator ⇒ Any] = aggToAccComp.map {
      case (agg, accComp) ⇒
        val get = accComp.get(agg)
        agg -> ((acc: Accumulator) ⇒ get(acc.asInstanceOf[accComp.A]))
    }

    new CombinedAccumulatorCreator {
      def getAggregations: Seq[Aggregation] = aggregations
      def create(): CombinedAccumulator = {
        val accsMap: Map[AccumulatorCompanion, Accumulator] = accComps.map(x ⇒ x._1 -> x._2())
        val accs: Seq[Accumulator] = accsMap.values.toList
        val thisGets: Map[Aggregation, () ⇒ Any] = gets.map {
          case (aggregation, get) ⇒
            val acc = accsMap(aggToAccComp(aggregation))
            aggregation -> (() ⇒ get(acc))
        }
        new CombinedAccumulator {
          def add(t: Long, value: Double): Unit = accs.foreach(_.add(t, value))
          def get(aggregation: Aggregation): Any = thisGets(aggregation)()
          def getAll: IndexedSeq[Any] = aggregations.iterator.map(get).toIndexedSeq
        }
      }
    }

  }
}

