package plutarch.client.experemental

import scala.scalajs.js
import js.annotation._

@js.native
@JSGlobal("Map")
class JSMap[K, V] extends js.Object {
  def size: Int = js.native
  def clear(): Unit = js.native
  def has(key: K): Boolean = js.native
  def get(key: K): js.UndefOr[V] = js.native
  def set(key: K, value: V): this.type = js.native
  def delete(key: K): Boolean = js.native
  def forEach(callback: js.Function2[V, K, Unit]): Unit = js.native
  def entries(): js.Iterator[js.Tuple2[K, V]] = js.native
  def keys(): js.Iterator[K] = js.native
  def values(): js.Iterator[V] = js.native
}

object JSMap {
  def apply[K, V](elems: (K, V)*): JSMap[K, V] = {
    val res = JSMap.empty[K, V]
    elems.foreach(elem ⇒ res.set(elem._1, elem._2))
    res
  }
  def empty[K, V]: JSMap[K, V] = new JSMap[K, V]
  @inline
  def getOrElseUpdate[K, V](map: JSMap[K, V], key: K, value: ⇒ V): V = {
    val uv = map.get(key)
    if (uv.isDefined) {
      uv.get
    } else {
      val v = value
      map.set(key, v)
      v
    }
  }
}