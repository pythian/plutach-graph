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

package plutarch.shared.data

object MetricDomain {

  /*
   *   Unit Selectors
   */

  sealed trait UnitSelector {
    def apply(sizeY: Double, maxValues: Int): Double
  }

  case object DefaultUnitSelector extends UnitSelector {
    def apply(sizeY: Double, maxValues: Int): Double = Math.pow(10, Math.log10(10 * sizeY / maxValues).floor)
  }

  case object BytesUnitSelector extends UnitSelector {
    def apply(sizeY: Double, maxValues: Int): Double = {
      val unit0 = sizeY / (maxValues - 2)
      var p1024 = Math.pow(1024, (Math.log(unit0) / Math.log(1024)).floor)
      var l10 = Math.log10(unit0 / p1024).ceil
      if (l10 == 3) {
        l10 = 0
        p1024 *= 1024
      }
      val res = Math.pow(10, l10) * p1024
      res
    }
  }

  /*
   *   Data Formats
   */

  sealed trait DataFormat {
    def apply(value: Double): String
  }

  case object DefaultDataFormat extends DataFormat {
    def apply(value: Double): String = toStr(value)
  }

  case object LongDataFormat extends DataFormat {
    def apply(value: Double): String = value.toLong.toString
  }

  case object LatencyDataFormat extends DataFormat {
    def apply(value: Double): String = {
      if (value.abs < 1000) s"${value.toInt} ms"
      else if (value.abs < 10000) f"${value / 1000}%.1f s"
      else if (value.abs < 60000) s"${(value / 1000).toInt} s"
      else s"${(value.abs / 60000).toInt} m"
    }
  }

  case object BytesDataFormat extends DataFormat {
    def apply(value: Double): String = {
      if (value > 0) {
        val l1024 = (Math.log(value) / Math.log(1024)).toInt
        val pre = "MGTPE".charAt(l1024)
        "%.1f %sB".format(value / Math.pow(1024, l1024), pre)
      } else "0"
    }
  }

  // converters

  // is there better way for default formatter than this ugly implementation?
  def toStr(d: Double): String = {
    if (d == 0) {
      "0"
    } else {
      val exp = math.log10(d.abs).floor
      val mant = d * math.pow(10, -exp)
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
        mantForm + "e" + exp
      }
    }
  }

}
