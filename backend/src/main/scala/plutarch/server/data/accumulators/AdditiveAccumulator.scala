package plutarch.server.data.accumulators

import plutarch.shared.data.Aggregations._

class AdditiveAccumulator extends Accumulator {
  //private var total = List.empty[Double]
  private var min: Double = 0.0
  private var max: Double = 0.0
  private var sum: Double = 0.0
  private var sum2: Double = 0.0
  private var count: Long = 0L
  def add(t: Long, value: Double): Unit = {
    if (count == 0) {
      min = value
      max = value
      sum = value
      sum2 = value * value
    } else {
      if (min > value) min = value
      else if (max < value) max = value
      sum += value
      sum2 += (value * value)
    }
    //total = value :: total
    count += 1
  }
  def getMin: Double = min
  def getMax: Double = max
  def getSum: Double = sum
  def getCount: Long = count
  def getAvg: Double = if (count == 0) 0.0 else sum / count
  def getStdev: Double = if (count == 0 || count == 1) 0.0 else {
    val std2 = (sum2 - sum * sum / count) / (count - 1)
    if (std2 < 0) 0.0 else Math.sqrt(std2)
  }
}

object AdditiveAccumulator extends AccumulatorCompanion {
  type A = AdditiveAccumulator
  def apply(): A = new AdditiveAccumulator()
  def getInstanceCreator(aggregations: Seq[Aggregation]): () ⇒ A = () ⇒ apply()
  val get: PartialFunction[Aggregation, A ⇒ Any] = {
    case Min   ⇒ _.getMin
    case Max   ⇒ _.getMax
    case Sum   ⇒ _.getSum
    case Count ⇒ _.getCount
    case Stdev ⇒ _.getStdev
    case Avg   ⇒ _.getAvg
  }
}