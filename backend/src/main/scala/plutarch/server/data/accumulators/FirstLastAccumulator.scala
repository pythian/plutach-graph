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