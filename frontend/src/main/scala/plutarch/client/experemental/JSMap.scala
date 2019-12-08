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

  class JSMapValues[V >: Null](underlying: JSMap[Double, V]) extends Values[V] with MValues[V] {

    override def set(k: Double, v: V): Unit = {
      underlying.set(k, v)
    }

    override def delete(k: Double): Unit = underlying.delete(k)

    override def getOrElse[T >: V](k: Double, default: T): T = {
      val u = underlying.get(k)
      if (u.isDefined) {
        u.asInstanceOf[V]
      } else {
        default
      }
    }

    override def from[T >: V](k: Double, step: Double, default: T): () ⇒ T = {
      var key = k
      () ⇒ {
        val res = getOrElse(key, default)
        key += step
        res
      }
    }

  }

  @inline
  def asValues[V >: Null](map: JSMap[Double, V]) = new JSMapValues(map)

}