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

