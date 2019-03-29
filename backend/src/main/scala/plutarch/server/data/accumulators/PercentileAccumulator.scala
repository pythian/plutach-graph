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

class PercentileAccumulator(percentiles: Seq[Double]) extends Accumulator {
  assert(percentiles.forall(x ⇒ 0 <= x && x <= 1))
  private class Elem(val value: Double, var left: Option[Elem], var right: Option[Elem], var count: Int = 1)
  private val elems = collection.mutable.TreeMap.empty[Double, Elem]
  private var head = Option.empty[Elem]
  private var length = 0
  private val pct = collection.mutable.Map.empty[Double, (Option[Elem], Int)] ++ percentiles.map(c ⇒ (c, (None, 0)))
  def add(t: Long, value: Double): Unit = {
    length += 1
    elems.get(value) match {
      case None ⇒
        val left = elems.rangeImpl(None, Some(value)).lastOption.map(_._2)
        val right = left.flatMap(_.right).orElse(elems.rangeImpl(Some(value), None).headOption.map(_._2))
        val elem = new Elem(value, left, right)
        left.foreach(_.right = Some(elem))
        right.foreach(_.left = Some(elem))
        elems.put(value, elem)
        if (left.isEmpty) head = Some(elem)
      case Some(elem) ⇒
        elem.count += 1
    }
    for (c ← percentiles) {
      pct(c) match {
        case (Some(cur), cnt) ⇒
          var newCnt = cnt
          if (value < cur.value) newCnt += 1
          val thisIndexLow = newCnt
          val thisIndexHigh = newCnt + cur.count - 1
          val targetIndex = if (c == 0) 0 else (c * length).ceil.toInt - 1
          if (targetIndex >= thisIndexLow && targetIndex <= thisIndexHigh) {
            pct(c) = (Some(cur), newCnt)
          } else if (targetIndex < thisIndexLow) {
            pct(c) = (cur.left, newCnt - cur.left.get.count)
          } else if (targetIndex > thisIndexHigh) {
            pct(c) = (cur.right, newCnt + cur.count)
          }
        case (None, _) ⇒
          pct(c) = (head, 0)
      }
    }
  }
  //  def getPercentileDesc(percentile: Double): Option[Double] = {
  //    pct(percentile)._1.map(_.value)
  //  }
  //  def getPercentileCont(percentile: Double): Option[Double] = {
  //    val idxCont = percentile * (length - 1)
  //    val coeff = idxCont - idxCont.floor
  //    pct(percentile)._1.map { x1 ⇒
  //      val x2 = x1.right.getOrElse(x1)
  //      x1.value * (1 - coeff) + x2.value * coeff
  //    }
  //  }
  //  def getCountDistinct: Option[Int] = if (elems.isEmpty) None else Some(elems.size)
  def getPercentileDesc(percentile: Double): Double = {
    pct(percentile)._1.map(_.value).get
  }
  def getPercentileCont(percentile: Double): Double = {
    val idxCont = percentile * (length - 1)
    val coeff = idxCont - idxCont.floor
    pct(percentile)._1.map { x1 ⇒
      val x2 = x1.right.getOrElse(x1)
      x1.value * (1 - coeff) + x2.value * coeff
    }.get
  }
  def getCountDistinct: Int = elems.size

  // test only
  def toIterator: Iterator[(Double, Int)] = new Iterator[(Double, Int)] {
    private var pointer = head
    def hasNext(): Boolean = pointer.isDefined
    def next(): (Double, Int) = {
      val res = (pointer.get.value, pointer.get.count)
      pointer = pointer.get.right
      res
    }
  }
  def percentileDescSlow(percentile: Double): Double = {
    val idx = if (percentile == 0) 0 else (percentile * length).ceil.toInt - 1
    val arr = toIterator.flatMap(x ⇒ (1 to x._2).map(_ ⇒ x._1)).toArray
    arr(idx)
  }
  def toIterator2: Iterator[(Double, Int)] = {
    elems.toIterator.map(x ⇒ (x._2.value, x._2.count))
  }
  def stdev: Double = {
    val avg = toIterator.map(x ⇒ x._2 * x._1).sum / length
    Math.sqrt(toIterator.map(x ⇒ (x._1 - avg) * (x._1 - avg) * x._2).sum / (length - 1))
  }
}

object PercentileAccumulator extends AccumulatorCompanion {
  type A = PercentileAccumulator
  def apply(percentiles: Seq[Double]): A = new PercentileAccumulator(percentiles)
  def getInstanceCreator(aggregations: Seq[Aggregation]): () ⇒ A = {
    val params = aggregations.flatMap {
      case Mean              ⇒ Seq(0.5)
      case PercentileCont(p) ⇒ Seq(p)
      case PercentileDesc(p) ⇒ Seq(p)
      case _                 ⇒ Nil
    }
    () ⇒ apply(params)
  }
  val get: PartialFunction[Aggregation, A ⇒ Any] = {
    case CountDistinct     ⇒ _.getCountDistinct
    case Mean              ⇒ _.getPercentileCont(0.5)
    case PercentileCont(p) ⇒ _.getPercentileCont(p)
    case PercentileDesc(p) ⇒ _.getPercentileDesc(p)
  }
}