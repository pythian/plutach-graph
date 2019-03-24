package plutarch.client.data

import CombinedDataView._
import DataView._
import plutarch.client.experemental.{ JSArray, JSMap }

object CombinedDataView {
  trait SimpleMap[-K, +V] {
    def get(key: K): V
    def contains(key: K): Boolean
  }
}

trait CombinedDataView {
  def reqLeft: Double
  def reqRight: Double
  def left: Double
  def right: Double
  def step: Double
  def bestScale: Int
  def values: SimpleMap[Double, collection.Map[Int, Double]]
  def info: Info

  def simple(hiddenObjects: Set[Int], transform: Double ⇒ Double): SimpleData = {
    val rangeBuilder = new RangeBuilder()
    val objects = JSMap.empty[Int, SDP]
    var key = left
    val ds = step
    while (key < right) {
      if (values.contains(key)) {
        val objs = values.get(key)
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