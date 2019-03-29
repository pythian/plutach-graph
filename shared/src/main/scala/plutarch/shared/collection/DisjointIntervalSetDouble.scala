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

package plutarch.shared.collection

import plutarch.shared.Protocol.Interval
import scala.collection.mutable.ListBuffer

/**
 * Disjoint [x, y) interval set.
 * Operations:
 *   getLength -- total length of all intervals
 *   add -- add 1, or many intervals
 *   remove -- remove interval from set
 *   intersect -- intersection of given interval with set
 *   diff -- set difference of interval and set
 *   shrink -- remove rawIntervals until get required length
 *
 */

// todo: add functionality to merge intervals in order to reduce rawIntervalsCount (we don't aim to have 10000 x 100 steps but better 100 x 10000 steps)
trait DisjointIntervalSetDouble {
  def rawIntervalsCount: Int
  def mergedIntervalsCount: Int
  def getLength: Double
  def getAll: Seq[Interval]
  def add(intervals: Iterable[Interval]): Unit = intervals.foreach(add)
  def add(left: Double, right: Double): Unit = add(Interval(left, right))
  def add(interval: Interval): Unit
  def remove(intervals: Iterable[Interval]): Unit = intervals.foreach(remove)
  def remove(left: Double, right: Double): Unit = remove(Interval(left, right))
  def remove(interval: Interval): Unit
  def intersect(left: Double, right: Double): Seq[Interval] = intersect(Interval(left, right))
  def intersect(interval: Interval): Seq[Interval]
  def diff(left: Double, right: Double): Seq[Interval] = diff(Interval(left, right))
  def diff(interval: Interval): Seq[Interval]
  def shrink(target: Double): Seq[Interval]
  def clear(): Unit
  def printLog(): Unit
}

object DisjointIntervalSetDouble {

  val oplog = false
  trait Operation {
    val tmsp: Long = System.currentTimeMillis()
  }
  case class OpAdd(interval: Interval) extends Operation
  case class OpRemove(interval: Interval) extends Operation
  case class OpUpdate(interval: Interval) extends Operation
  case class OpRemoveLast() extends Operation
  case class OpClear() extends Operation
  case class OpDiff(interval: Interval, res: Seq[Interval]) extends Operation
  case class OpShrink(target: Double, res: Seq[Interval]) extends Operation

  def empty: DisjointIntervalSetDouble = new Impl()

  class Impl extends DisjointIntervalSetDouble {
    private var length = 0.0
    private val deleteQueue = LinkedSet.empty[Interval]
    private val rawIntervals = RedBlackTreeDouble.empty[Interval]
    private val mergedIntervals = RedBlackTreeDouble.empty[Interval]

    private val operations = scala.collection.mutable.ArrayBuffer.empty[Operation]

    def rawIntervalsCount: Int = rawIntervals.size
    def mergedIntervalsCount: Int = mergedIntervals.size
    def getAll: Seq[Interval] = RedBlackTreeDouble.iterator(mergedIntervals).map(_._2).toList

    private def logop(op: ⇒ Operation): Unit = if (oplog) operations += op

    def printLog(): Unit = {
      operations.foreach { op ⇒
        println(s"tmpsp=${op.tmsp}, op=$op")
      }
    }

    override def toString: String = {
      s"""{"length": $length, "deleteQueue": ${deleteQueue.toString}, "rawIntervals": ${RedBlackTreeDouble.toString(rawIntervals)}, "mergedIntervals": ${RedBlackTreeDouble.toString(mergedIntervals)}}"""
    }

    def getLength: Double = length

    def add(interval: Interval): Unit = {
      if (oplog) operations += OpAdd(interval)

      length += (interval.right - interval.left)
      deleteQueue.add(interval)
      RedBlackTreeDouble.insert(rawIntervals, interval.left, interval)

      val rightIntervalNode = RedBlackTreeDouble.minNodeAfter(mergedIntervals, interval.left)
      val leftIntervalNode = if (rightIntervalNode ne null) rightIntervalNode.prev else RedBlackTreeDouble.maxNodeBefore(mergedIntervals, interval.left)

      if ((leftIntervalNode ne null) && (rightIntervalNode ne null)) {
        val leftInterval = leftIntervalNode.value
        val rightInterval = rightIntervalNode.value
        // 4 cases: non-mergable, both-mergable, left-mergable, right-mergable
        if (leftInterval.right < interval.left && interval.right < rightInterval.left) {
          RedBlackTreeDouble.insert(mergedIntervals, interval.left, interval)
        } else if (leftInterval.right == interval.left && interval.right == rightInterval.left) {
          RedBlackTreeDouble.delete(mergedIntervals, leftInterval.left)
          RedBlackTreeDouble.delete(mergedIntervals, rightInterval.left)
          RedBlackTreeDouble.insert(mergedIntervals, leftInterval.left, Interval(leftInterval.left, rightInterval.right))
        } else if (leftInterval.right == interval.left && interval.right < rightInterval.left) {
          RedBlackTreeDouble.insert(mergedIntervals, leftInterval.left, Interval(leftInterval.left, interval.right))
        } else if (leftInterval.right < interval.left && interval.right == rightInterval.left) {
          RedBlackTreeDouble.delete(mergedIntervals, rightInterval.left)
          RedBlackTreeDouble.insert(mergedIntervals, interval.left, Interval(interval.left, rightInterval.right))
        } else {
          sys.error(s"incorrect interval adding [$leftInterval, $interval, $rightInterval]")
        }
      } else if ((leftIntervalNode eq null) && (rightIntervalNode ne null)) {
        val rightInterval = rightIntervalNode.value
        // 2 cases: non-mergable, right-mergable
        if (interval.right < rightInterval.left) {
          RedBlackTreeDouble.insert(mergedIntervals, interval.left, interval)
        } else if (interval.right == rightInterval.left) {
          RedBlackTreeDouble.delete(mergedIntervals, rightInterval.left)
          RedBlackTreeDouble.insert(mergedIntervals, interval.left, Interval(interval.left, rightInterval.right))
        } else {
          sys.error(s"incorrect interval adding [$interval, $rightInterval]")
        }
      } else if ((leftIntervalNode ne null) && (rightIntervalNode eq null)) {
        val leftInterval = leftIntervalNode.value
        // 2 cases: non-mergable, left-mergable
        if (leftInterval.right < interval.left) {
          RedBlackTreeDouble.insert(mergedIntervals, interval.left, interval)
        } else if (leftInterval.right == interval.left) {
          RedBlackTreeDouble.insert(mergedIntervals, leftInterval.left, Interval(leftInterval.left, interval.right))
        } else {
          sys.error(s"incorrect interval adding [$leftInterval, $interval]")
        }
      } else {
        RedBlackTreeDouble.insert(mergedIntervals, interval.left, interval)
      }
    }

    private def removeLast(): Interval = {
      logop(OpRemoveLast())

      val interval = deleteQueue.getLast
      remove(interval)
      interval
    }

    def remove(interval: Interval): Unit = {
      logop(OpRemove(interval))

      length -= (interval.right - interval.left)
      deleteQueue.remove(interval)
      RedBlackTreeDouble.delete(rawIntervals, interval.left)
      val leftIntervalNode = RedBlackTreeDouble.maxNodeBefore(mergedIntervals, interval.left)
      if (leftIntervalNode ne null) {
        val toSplit = leftIntervalNode.value
        if (toSplit.left == interval.left && interval.right == toSplit.right) {
          RedBlackTreeDouble.delete(mergedIntervals, interval.left)
        } else if (toSplit.left == interval.left && interval.right < toSplit.right) {
          RedBlackTreeDouble.delete(mergedIntervals, toSplit.left)
          RedBlackTreeDouble.insert(mergedIntervals, interval.right, Interval(interval.right, toSplit.right))
        } else if (toSplit.left < interval.left && interval.right == toSplit.right) {
          RedBlackTreeDouble.insert(mergedIntervals, toSplit.left, Interval(toSplit.left, interval.left))
        } else if (toSplit.left < interval.left && interval.right < toSplit.right) {
          RedBlackTreeDouble.insert(mergedIntervals, toSplit.left, Interval(toSplit.left, interval.left))
          RedBlackTreeDouble.insert(mergedIntervals, interval.right, Interval(interval.right, toSplit.right))
        } else {
          sys.error(s"incorrect interval remove [$interval, $toSplit]")
        }
      } else {
        sys.error(s"incorrect interval remove [$interval]")
      }
    }

    def intersect(interval: Interval): Seq[Interval] = {
      var node = RedBlackTreeDouble.maxNodeBeforeOrFirst(mergedIntervals, interval.left)
      val lb = ListBuffer.empty[Interval]
      while ((node ne null) && node.key <= interval.right) {
        val covering = node.value
        if (covering.left >= interval.left && covering.right <= interval.right) {
          lb += covering
        } else if (covering.left < interval.left && covering.right > interval.left) {
          lb += Interval(interval.left, covering.right)
        } else if (covering.left < interval.right && covering.right > interval.right) {
          lb += Interval(covering.left, interval.right)
        } else if (covering.left <= interval.left && covering.right >= interval.right) {
          lb += interval
        }
        node = node.next
      }
      lb.result()
    }

    def diff(interval: Interval): Seq[Interval] = {
      var node = RedBlackTreeDouble.maxNodeBeforeOrFirst(mergedIntervals, interval.left)
      var cur = interval.left
      val lb = ListBuffer.empty[Interval]
      while ((node ne null) && node.key <= interval.right) {
        val covering = node.value
        if (covering.left > cur) {
          lb += Interval(cur, covering.left)
        }
        if (covering.right > cur) {
          cur = covering.right
        }
        node = node.next
      }
      if (cur < interval.right) lb += Interval(cur, interval.right)
      update(interval)
      logop(OpDiff(interval, lb.result()))
      lb.result()
    }

    // moving intersected with interval to queue tail
    private def update(interval: Interval): Unit = {
      logop(OpUpdate(interval))
      var node = RedBlackTreeDouble.maxNodeBeforeOrFirst(rawIntervals, interval.left)
      while ((node ne null) && node.key < interval.right) {
        deleteQueue.refresh(node.value)
        node = node.next
      }
    }

    def shrink(target: Double): Seq[Interval] = {
      val res = ListBuffer.empty[Interval]
      while (getLength > target) {
        res += removeLast()
      }
      logop(OpShrink(target, res.result()))
      res.result()
    }

    def clear(): Unit = {
      logop(OpClear())
      length = 0.0
      deleteQueue.clear()
      RedBlackTreeDouble.clear(rawIntervals)
      RedBlackTreeDouble.clear(mergedIntervals)
    }
  }
}
