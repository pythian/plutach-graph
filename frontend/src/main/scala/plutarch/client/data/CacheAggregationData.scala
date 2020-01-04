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

package plutarch.client.data

import plutarch.shared.Protocol
import plutarch.shared.Protocol.Interval
import plutarch.shared.collection.RedBlackTreeInt
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers
import plutarch.shared.data.metrics.Meta
import slogging.LazyLogging

object CacheAggregationData {
  def create(
    cacheState:  CacheState,
    meta:        Meta,
    aggregation: Aggregation,
    dataSource:  DataSource): CacheAggregationData =
    new CacheAggregationData(cacheState, meta, aggregation, dataSource)
}

class CacheAggregationData(
    cacheState:  CacheState,
    meta:        Meta,
    aggregation: Aggregation,
    dataSource:  DataSource) extends AggregationData with LazyLogging {

  private val cacheScales = RedBlackTreeInt.empty[CacheScale]
  for (scale ← meta.conf.scales) {
    RedBlackTreeInt.insert(cacheScales, scale, CacheScale.create(scale, cacheState, meta, aggregation, dataSource))
  }

  def getExact(targetScale: Int, x: Double, y: Double): DataView = {
    val targetNode = RedBlackTreeInt.maxNodeBeforeOrFirst(cacheScales, targetScale)
    val cacheScale = targetNode.value
    val bestScale = targetNode.key
    cacheScale.get(x, y, bestScale, isRequested = true)
      .getOrElse(DataView.empty(targetNode.key * meta.conf.step, x, y, bestScale, cacheScale.info))
  }

  private var prev: (Double, Double) = (Double.MinValue, Double.MaxValue)
  private var prevIsZoomIn: Boolean = true
  private var currently: Int = 0
  private var prevBestScale: Int = 0

  def get(targetScale: Int, x: Double, y: Double): DataView = {

    val targetNode = RedBlackTreeInt.maxNodeBeforeOrFirst(cacheScales, targetScale)
    val cacheScale = targetNode.value
    val bestScale = targetNode.key

    // predictive zooming caching
    if (prevBestScale != bestScale) {
      currently = 0
    } else if (prev._1 < x && y < prev._2) {
      //logger.info("isZoomIn=true")
      currently = if (prevIsZoomIn) currently + 1 else 1
      if (currently >= 3) {
        if (targetNode.prev != null) {
          targetNode.prev.value.request(x, y)
        }
      }
      prevIsZoomIn = true
    } else if (x < prev._1 && prev._2 < y) {
      currently = if (!prevIsZoomIn) currently + 1 else 1
      val k = (prev._2 - prev._1) / (y - x)
      if (currently >= 3 && k < 3) {
        if (targetNode.next != null) {
          val c = (prev._2 - k * y) / (1 - k)
          val (x2, y2) = (c + k * 1.3 * (x - c), c + k * 1.3 * (y - c))
          targetNode.next.value.request(x2, y2)
        }
      }
      prevIsZoomIn = false
    }
    prev = (x, y)
    prevBestScale = bestScale

    cacheScale.get(x, y, bestScale, isRequested = true) match {
      case Some(view) ⇒ view // 1. get exact
      case None ⇒
        // try to use current scale when scale up if best of not available, as current is likely cached
        var node = if (targetNode.prev != null) {
          targetNode.prev
        } else {
          RedBlackTreeInt.minNodeAfter(cacheScales, targetScale)
        }
        var res = Option.empty[DataView]
        while ((node ne null) && res.isEmpty) {
          if (node.key != targetScale) { // skip key as we already checked it (in case targetScale is exact match to it)
            res = node.value.get(x, y, bestScale, isRequested = node != targetNode.prev)
          }
          node = node.next
        }
        res match {
          case Some(view) ⇒ view // 2. find first with higher scale
          case None ⇒
            var node = targetNode
            var res = Option.empty[DataView]
            while ((node ne null) && res.isEmpty) {
              res = node.value.getAny(x, y, bestScale, 0.7)
              node = node.next
            }
            res match {
              case Some(view) ⇒ view // 3. get any covering > 0.7 of (x, y)
              case None       ⇒ DataView.empty(targetNode.key * meta.conf.step, x, y, bestScale, cacheScale.info) // 4. get empty
            }
        }
    }
  }

  def receive(req: Protocol.WSHistRequest, data: Picklers.CombinedData[_]): Unit = {
    RedBlackTreeInt.getNode(cacheScales, req.scale).value.receive(req, data)
  }
  def receive(rawKey: Long, data: Protocol.CurrentData): Unit = {
    RedBlackTreeInt.getNode(cacheScales, data.scale).value.receive(rawKey, data)
  }
  def updateCurrent(current: Long): Unit = {
    for ((_, cacheScale) ← RedBlackTreeInt.iterator(cacheScales)) {
      cacheScale.updateCurrent(current)
    }
  }
  def getAll: Seq[(Int, Seq[Interval])] = RedBlackTreeInt.iterator(cacheScales).map(x ⇒ x._1 -> x._2.getAll).toList
  def getRequested: Seq[(Int, Seq[Interval])] = RedBlackTreeInt.iterator(cacheScales).map(x ⇒ x._1 -> x._2.getRequested).toList
  def reset(): Unit = {
    for ((_, cacheScale) ← RedBlackTreeInt.iterator(cacheScales)) {
      cacheScale.reset()
    }
  }
}
