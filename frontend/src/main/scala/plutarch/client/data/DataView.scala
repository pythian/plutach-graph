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

import plutarch.client.experemental.{ JSArray, JSMap, JSSet }
import scala.collection.Map

/*
* For given metric, and given aggregation key->[objects->value]
* view of data between left and right for keys with step
* To avoid unnecessary complexity, there is no joined data for different, so all keys are 'left <= left + k * step <= right'
* */

import DataView._

object DataView {

  trait Info

  def empty(step: Double, reqLeft: Double, reqRight: Double, bestScale: Int, info: Info): DataView =
    EmptyDataView(step, reqLeft, reqRight, bestScale, info)

  case class EmptyDataView(step: Double, reqLeft: Double, reqRight: Double, bestScale: Int, info: Info) extends DataView {
    def left: Double = 0
    def right: Double = -1
    def values: JSMap[Double, Map[Int, Double]] = JSMap.empty
  }

  type SDP = JSArray[SimpleDataPoint]
  type LDP = JSArray[LineDataPoint]
  type DP = JSArray[DataPoint]

  class RangeBuilder() {
    private var maxY = Double.MinValue
    private var minY = Double.MaxValue
    def add(value: Double): Unit = {
      minY = minY min value
      maxY = maxY max value
    }
    def result(): Option[Range] = {
      if (minY != Double.MaxValue || maxY != Double.MinValue) Some(Range(minY, maxY)) else None
    }
  }

  case class Range(min: Double, max: Double)
  case class DataPoint(key: Double, value: Double)
  case class StackedData(data: JSMap[Int, DP], range: Option[Range])
  case class LineDataPoint(key: Double, value: Double, isAppearing: Boolean, var isDisappearing: Boolean = false)
  case class LineData(data: JSMap[Int, LDP], range: Option[Range])
  case class SimpleDataPoint(key: Double, value: Double)
  case class SimpleData(data: JSMap[Int, SDP], range: Option[Range])
  case class Interpolation()
  case class Integral()

  // test purposes
  def toLineData(stackedData: StackedData): LineData = {
    val newData = JSMap.empty[Int, LDP]
    stackedData.data.forEach { (v, k) ⇒
      val nv = JSArray.empty[LineDataPoint]
      var first = true
      v.forEach { dp ⇒
        nv.push(LineDataPoint(dp.key, dp.value, first, false))
        first = false
      }
      newData.set(k, nv)
    }
    LineData(newData, stackedData.range)
  }

}

trait DataView {
  def reqLeft: Double
  def reqRight: Double
  def left: Double
  def right: Double
  def step: Double
  def bestScale: Int
  def values: JSMap[Double, Map[Int, Double]]
  def info: Info

  def interpolation: Interpolation = ???
  def total(transform: Double ⇒ Double): LineData = ???
  def integral: Integral = ???

  private def getPrev(objIds: JSSet[Int], order: Int ⇒ Int, thisId: Int, objs: JSMap[Int, Double]): Double = {
    val thisOrder = order(thisId)
    var curObj = Option.empty[Int]
    var curObjOrder = Option.empty[Int]
    objIds.forEach { objId ⇒
      val objOrder = order(objId)
      if (objOrder < thisOrder && (curObjOrder.isEmpty || curObjOrder.get < objOrder)) {
        curObj = Some(objId)
        curObjOrder = Some(objOrder)
      }
    }
    curObj match {
      case Some(objId) if objs.has(objId) ⇒ objs.get(objId).asInstanceOf[Double]
      case _                              ⇒ 0.0
    }
  }

  def stacked(order: Int ⇒ Int, hiddenObjects: Set[Int], transform: Double ⇒ Double): StackedData = {
    val rangeBuilder = new RangeBuilder()
    val objects = JSMap.empty[Int, DP]
    var prevKey = Double.MinValue
    var prevObjs = JSSet.empty[Int]
    var prevObjsValues = JSMap.empty[Int, Double]
    var key = left
    while (key < right) {
      val uobjs = values.get(key)
      if (uobjs.isDefined) {
        val objs = uobjs.asInstanceOf[Map[Int, Double]]
        val currObjs = JSSet.empty[Int]
        val orderedObjs = JSArray.empty[Int]
        objs.keys.foreach { objId ⇒
          if (objId > 0 && !hiddenObjects(objId)) {
            currObjs.add(objId)
            orderedObjs.push(objId)
          }
        }
        var currValue = 0.0
        val currObjValues = JSMap.empty[Int, Double]
        orderedObjs.sort((id1, id2) ⇒ order(id1).compareTo(order(id2))).forEach { objId ⇒
          currValue += transform(objs(objId))
          currObjValues.set(objId, currValue)
          val arr = JSMap.getOrElseUpdate[Int, DP](objects, objId, JSArray.empty)
          if (!prevObjs.has(objId) && prevKey != Double.MinValue) {
            val value = getPrev(currObjs, order, objId, prevObjsValues)
            arr.push(DataPoint(prevKey, 0.0))
            arr.push(DataPoint(prevKey, value))
          } else {
            prevObjs.delete(objId)
          }
          arr.push(DataPoint(key, currValue))
        }
        rangeBuilder.add(currValue)
        prevObjs.forEach { objId ⇒
          val value = getPrev(currObjs, order, objId, currObjValues)
          val arr = objects.get(objId).asInstanceOf[DP]
          arr.push(DataPoint(key, value))
          arr.push(DataPoint(key, 0.0))
        }
        prevObjs = currObjs
        prevObjsValues = currObjValues
      } else {
        prevObjs.forEach { objId ⇒
          objects.get(objId).asInstanceOf[DP].push(DataPoint(key, 0.0))
        }
        prevObjs.clear()
        prevObjsValues.clear()
      }
      prevKey = key
      key += step
    }
    StackedData(objects, rangeBuilder.result())
  }

  def line(hiddenObjects: Set[Int], transform: Double ⇒ Double): LineData = {
    val rangeBuilder = new RangeBuilder()
    val objects = JSMap.empty[Int, LDP]
    var prevObjs = JSSet.empty[Int]
    var key = left
    val ds = step
    while (key < right) {
      val uobjs = values.get(key)
      if (uobjs.isDefined) {
        val objs = uobjs.asInstanceOf[Map[Int, Double]]
        val currObjs = JSSet.empty[Int]
        objs.foreach {
          case (objId, value) ⇒
            if (objId > 0 && !hiddenObjects(objId)) {
              val tvalue = transform(value)
              rangeBuilder.add(tvalue)
              currObjs.add(objId)
              val arr = JSMap.getOrElseUpdate[Int, LDP](objects, objId, JSArray.empty)
              if (!prevObjs.has(objId)) {
                arr.push(LineDataPoint(key, tvalue, isAppearing = true))
              } else {
                prevObjs.delete(objId)
                arr.push(LineDataPoint(key, tvalue, isAppearing = false))
              }
            }
        }
        prevObjs.forEach { objId ⇒
          val arr = objects.get(objId).asInstanceOf[LDP]
          arr(arr.length - 1).isDisappearing = true
        }
        prevObjs = currObjs
      } else {
        prevObjs.forEach { objId ⇒
          val arr = objects.get(objId).asInstanceOf[LDP]
          arr(arr.length - 1).isDisappearing = true
        }
        prevObjs.clear()
      }
      key += ds
    }
    LineData(objects, rangeBuilder.result())
  }

  def simple(hiddenObjects: Set[Int], transform: Double ⇒ Double): SimpleData = {
    val rangeBuilder = new RangeBuilder()
    val objects = JSMap.empty[Int, SDP]
    var key = left
    val ds = step
    while (key < right) {
      if (values.has(key)) {
        val objs = values.get(key).asInstanceOf[Map[Int, Double]]
        objs.foreach {
          case (objId, value) ⇒
            if (objId > 0 && !hiddenObjects(objId)) {
              val tvalue = transform(value)
              rangeBuilder.add(tvalue)
              val arr = JSMap.getOrElseUpdate[Int, SDP](objects, objId, JSArray.empty)
              arr.push(SimpleDataPoint(key, tvalue))
            }
        }
      }
      key += ds
    }
    SimpleData(objects, rangeBuilder.result())
  }

}