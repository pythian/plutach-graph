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

package plutarch.client.graph

import rx._
import org.scalajs.dom.html.Div
import plutarch.client.experemental.DateTools
import scalatags.JsDom.all._
import plutarch.client.experemental.RxTools._

class Tables(
    controlCursor: Cursor,
    selecting:     Selecting,
    graph:         Graph,
    conf:          GraphControlConf,
    state:         GraphControlState)(
    implicit
    ctx: Ctx.Owner, geometry: Geometry.Context) {

  def toStr(d: Double): String = {
    if (d == 0) {
      "0"
    } else {
      val exp = Math.floor(Math.log10(Math.abs(d)))
      val mant = d * Math.pow(10, -exp)
      if (exp >= -3 && exp <= 3) {
        val s = d.toString
        val i = s.indexOf(".")
        if (i > 0) {
          val res = s.take(i + 5).reverse.dropWhile(_ == '0').reverse
          if (res.takeRight(1) == ".") res.dropRight(1) else res
        } else {
          s
        }
      } else {
        val mantForm = "%.4f".format(mant).toDouble
        s"${mantForm}e$exp"
      }
    }
  }

  def toStr2(d: Double, expLimit: Int): String = {
    if (d == 0) {
      "0"
    } else {
      val exp = Math.floor(Math.log10(Math.abs(d)))
      val mant = d * Math.pow(10, -exp)
      if (exp >= -expLimit && exp <= expLimit) {
        val s = d.toString
        val i = s.indexOf(".")
        if (i > 0) {
          val res = s.take(i + 5).reverse.dropWhile(_ == '0').reverse
          if (res.takeRight(1) == ".") res.dropRight(1) else res
        } else {
          s
        }
      } else {
        val mantForm = "%.4f".format(mant).toDouble
        s"${mantForm}e$exp"
      }
    }
  }

  lazy val cursorTableContent: Div = {
    val rows =
      controlCursor.rxPos.map(pos ⇒ for {
        _ ← pos.toSeq
        p ← controlCursor.getPos.map(_.asUniverse).toSeq
        interpolation ← graph.interpolate(p).toSeq
        mconf ← state.metric.toSeq
        total = interpolation.data.map(_._2).foldLeft(0.0)(_ + _)
        (id, value) ← interpolation.data
      } yield {
        val pct = 100.0 * value / total
        val obj = conf.data.get(mconf.name).getObjectById(id)
        tr(td(obj.getName, backgroundColor := obj.getColor1), td(toStr2(value, 6)), td(toStr2(pct, 6)))
      })
    val bdy = rows.into(tbody().render)
    div(table(
      thead(tr(th(pre("Object" + (" " * 30))), th(pre("Value      ")), th(pre("Pct      ")))),
      bdy)).render
  }

  private def getNumColor(idx: Int) = {
    if (idx % 2 == 0) {
      "#F9F9F5"
    } else {
      "#F9F5F9"
    }
  }

  lazy val selectTableContent: Div = {
    val rows =
      selecting.rxInterval.map(pos ⇒ for {
        (x1, x2) ← pos.toSeq
        integral ← graph.integral(x1, x2).toSeq
        mconf ← state.metric.toSeq
        total = integral.data.map(_._1._2).foldLeft(0.0)(_ + _)
        ((id, value), idx) ← integral.data
      } yield {
        val pct = 100.0 * value / total
        val obj = conf.data.get(mconf.name).getObjectById(id)
        tr(td(idx, backgroundColor := getNumColor(idx)), td(obj.getName, backgroundColor := obj.getColor1), td(toStr2(value, 6)), td(toStr2(pct, 6)))
      })
    val bdy = rows.into(tbody().render)
    div(table(
      thead(tr(th(pre("№  ")), th(pre("Object" + (" " * 30))), th(pre("Value      ")), th(pre("Pct      ")))),
      bdy)).render
  }

  lazy val component: Div = {
    val rxPosTime = controlCursor
      .rxPos
      .map(_ ⇒ controlCursor.getPos.map(_.asUniverse.x)
        .map(d ⇒ DateTools.toDateStr(d, withMillis = false)).getOrElse(""))
      .map(d ⇒ pre(s"Cursor: $d"))

    val rxSelectTime = selecting.rxInterval.map(o ⇒ o.map(pos ⇒ {
      val x1 = DateTools.toDateStr(pos._1, withMillis = false)
      val x2 = DateTools.toDateStr(pos._2, withMillis = false)
      s"[$x1, $x2]"
    }).getOrElse("")).map(d ⇒ pre(s"Select: $d"))

    div(
      table(
        thead(tr(th(rxPosTime, backgroundColor := "#F5F8F5"), th(rxSelectTime, backgroundColor := "#F5F5F8"))),
        tbody(tr(td(cursorTableContent, verticalAlign := "top"), td(selectTableContent, verticalAlign := "top"))))).render
  }

}
