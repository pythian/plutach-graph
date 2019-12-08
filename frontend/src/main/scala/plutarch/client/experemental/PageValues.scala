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

class PageValues[V >: Null](val pagesize: Int, val step: Double) extends Values[V] with MValues[V] {

  class Page(capacity: Int) {
    var size: Int = 1
    val array: JSArray[js.UndefOr[V]] = new JSArray(capacity)
  }

  private val pages = JSMap.empty[Double, Page]

  override def getOrElse[T >: V](k: Double, default: T): T = {
    val kNorm = k / step
    val pageNo = (kNorm / pagesize).floor
    val uPage = pages.get(pageNo)
    if (uPage.isDefined) {
      val idx = (kNorm - pageNo * pagesize).toInt
      val uElem = uPage.asInstanceOf[Page].array(idx)
      if (uElem != null && uElem.isDefined) {
        uElem.asInstanceOf[V]
      } else {
        default
      }
    } else {
      default
    }
  }

  override def from[T >: V](k: Double, step: Double, default: T): () ⇒ T = {
    val kNorm = k / step
    var pageNo = (kNorm / pagesize).floor
    var idx = (kNorm - pageNo * pagesize).toInt
    var uPage = pages.get(pageNo)
    () ⇒ {
      if (idx == pagesize) {
        idx = 0
        pageNo = pageNo + 1
        uPage = pages.get(pageNo)
      }
      if (uPage.isDefined) {
        val uElem = uPage.asInstanceOf[Page].array(idx)
        idx += 1
        if (uElem != null && uElem.isDefined) {
          uElem.asInstanceOf[V]
        } else {
          default
        }
      } else {
        idx += 1
        default
      }
    }
  }

  override def set(k: Double, v: V): Unit = {
    val kNorm = k / step
    val pageNo = (kNorm / pagesize).floor
    val idx = (kNorm - pageNo * pagesize).toInt
    val uPage = pages.get(pageNo)
    if (uPage.isDefined) {
      val page = uPage.asInstanceOf[Page]
      page.size += 1
      page.array(idx) = v
    } else {
      val page = new Page(pagesize)
      pages.set(pageNo, page)
      page.array(idx) = v
    }
  }

  override def delete(k: Double): Unit = {
    val kNorm = k / step
    val pageNo = (kNorm / pagesize).floor
    val idx = (kNorm - pageNo * pagesize).toInt
    val page = pages.get(pageNo).asInstanceOf[Page]
    page.size -= 1
    page.array(idx) = null
    if (page.size == 0) {
      pages.delete(pageNo)
    }
  }

}

object PageValues {
  def apply[V >: Null](pagesize: Int, step: Double): PageValues[V] = new PageValues(pagesize, step)
}