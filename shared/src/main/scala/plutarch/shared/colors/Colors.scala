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

package plutarch.shared.colors

import java.util.concurrent.atomic.AtomicLong

import scala.util.Random
import scala.collection.mutable.{ Queue ⇒ MQueue }

trait Colors {
  def next(): String
}

object Colors {

  def hex2rgb(c: String): (Int, Int, Int) = {
    val r = Integer.parseInt(c.slice(1, 3), 16)
    val g = Integer.parseInt(c.slice(3, 5), 16)
    val b = Integer.parseInt(c.slice(5, 7), 16)
    (r, g, b)
  }

  def rgb2hex(c: (Int, Int, Int)): String = {
    val (r, g, b) = c
    def hex(x: Int) = "%02x".format(x)
    s"#${hex(r)}${hex(g)}${hex(b)}"
  }

  def rgb2hsl(c: (Int, Int, Int)): (Double, Double, Double) = {
    val (rint, gint, bint) = c
    var (h, s, l) = (0d, 0d, 0d)
    val r = rint.toDouble / 255d
    val g = gint.toDouble / 255d
    val b = bint.toDouble / 255d
    val var_min = r.min(g).min(b)
    val var_max = r.max(g).max(b)
    val del_max = var_max - var_min
    l = (var_max + var_min) / 2
    if (del_max == 0) {
      h = 0
      s = 0
    } else {
      if (l < 0.5) {
        s = del_max / (var_max + var_min)
      } else {
        s = del_max / (2 - var_max - var_min)
      }
      val del_r = (((var_max - r) / 6) + (del_max / 2)) / del_max
      val del_g = (((var_max - g) / 6) + (del_max / 2)) / del_max
      val del_b = (((var_max - b) / 6) + (del_max / 2)) / del_max

      if (r == var_max) {
        h = del_b - del_g
      } else if (g == var_max) {
        h = (1 / 3) + del_r - del_b
      } else if (b == var_max) {
        h = (2 / 3) + del_g - del_r
      }

      if (h < 0) {
        h += 1
      }

      if (h > 1) {
        h -= 1
      }
    }
    (h, s, l)
  }

  def hue2rgb(v1: Double, v2: Double, inputvh: Double): Double = {
    val vh = if (inputvh < 0) inputvh + 1 else if (inputvh > 1) inputvh - 1 else inputvh
    if ((6 * vh) < 1) {
      v1 + (v2 - v1) * 6 * vh
    } else if ((2 * vh) < 1) {
      v2
    } else if ((3 * vh) < 2) {
      v1 + (v2 - v1) * ((2d / 3d - vh) * 6)
    } else v1
  }

  def hsl2rgb(c: (Double, Double, Double)): (Int, Int, Int) = {
    val (h, s, l) = c
    if (s == 0) {
      val gr = (l * 255).toInt
      (gr, gr, gr)
    } else {
      val var_2 = if (l < 0.5) {
        l * (1 + s)
      } else {
        (l + s) - (s * l)
      }

      val var_1 = 2 * l - var_2
      val r = 255 * hue2rgb(var_1, var_2, h + (1d / 3d))
      val g = 255 * hue2rgb(var_1, var_2, h)
      val b = 255 * hue2rgb(var_1, var_2, h - (1d / 3d))
      (r.toInt, g.toInt, b.toInt)
    }
  }

  def distCIEDE2000(L1: Double, a1: Double, b1: Double, L2: Double, a2: Double, b2: Double): Double = {
    val Lmean = (L1 + L2) / 2d
    val C1 = Math.sqrt(a1 * a1 + b1 * b1)
    val C2 = Math.sqrt(a2 * a2 + b2 * b2)
    val Cmean = (C1 + C2) / 2d

    val G = (1 - Math.sqrt(Math.pow(Cmean, 7) / (Math.pow(Cmean, 7) + Math.pow(25, 7)))) / 2
    val a1prime = a1 * (1 + G)
    val a2prime = a2 * (1 + G)

    val C1prime = Math.sqrt(a1prime * a1prime + b1 * b1)
    val C2prime = Math.sqrt(a2prime * a2prime + b2 * b2)
    val Cmeanprime = (C1prime + C2prime) / 2d

    val h1prime = Math.atan2(b1, a1prime) + 2 * Math.PI * (if (Math.atan2(b1, a1prime) < 0) 1 else 0)
    val h2prime = Math.atan2(b2, a2prime) + 2 * Math.PI * (if (Math.atan2(b1, a2prime) < 0) 1 else 0)
    val Hmeanprime = if (Math.abs(h1prime - h2prime) > Math.PI) (h1prime + h2prime + 2 * Math.PI) / 2d else (h1prime + h2prime) / 2d

    val T = 1d - 0.17d * Math.cos(Hmeanprime - Math.PI / 6.0) + 0.24 * Math.cos(2 * Hmeanprime) + 0.32 * Math.cos(3 * Hmeanprime + Math.PI / 30) - 0.2 * Math.cos(4 * Hmeanprime - 21 * Math.PI / 60)

    val deltahprime = if (Math.abs(h1prime - h2prime) <= Math.PI) h2prime - h1prime else if (h2prime <= h1prime) h2prime - h1prime + 2 * Math.PI else h2prime - h1prime - 2 * Math.PI

    val deltaLprime = L2 - L1
    val deltaCprime = C2prime - C1prime
    val deltaHprime = 2d * Math.sqrt(C1prime * C2prime) * Math.sin(deltahprime / 2d)
    val SL = 1d + (0.015d * (Lmean - 50d) * (Lmean - 50d)) / Math.sqrt(20d + (Lmean - 50d) * (Lmean - 50d))
    val SC = 1d + 0.045d * Cmeanprime
    val SH = 1d + 0.015d * Cmeanprime * T

    val deltaTheta = (30 * Math.PI / 180) * Math.exp(-((180 / Math.PI * Hmeanprime - 275) / 25) * ((180 / Math.PI * Hmeanprime - 275) / 25))
    val RC = 2 * Math.sqrt(Math.pow(Cmeanprime, 7) / (Math.pow(Cmeanprime, 7) + Math.pow(25, 7)))
    val RT = -RC * Math.sin(2 * deltaTheta)

    val KL = 1
    val KC = 1
    val KH = 1

    val deltaE = Math.sqrt(
      ((deltaLprime / (KL * SL)) * (deltaLprime / (KL * SL))) +
        ((deltaCprime / (KC * SC)) * (deltaCprime / (KC * SC))) +
        ((deltaHprime / (KH * SH)) * (deltaHprime / (KH * SH))) +
        (RT * (deltaCprime / (KC * SC)) * (deltaHprime / (KH * SH))))

    deltaE
  }

  def rgb2xyz(R: Double, G: Double, B: Double): (Double, Double, Double) = {
    val r = if (R <= 0.04045d) R / 12.92 else Math.pow((R + 0.055d) / 1.055d, 2.4d)
    val g = if (G <= 0.04045d) G / 12.92 else Math.pow((G + 0.055d) / 1.055d, 2.4d)
    val b = if (B <= 0.04045d) B / 12.92 else Math.pow((B + 0.055d) / 1.055d, 2.4d)

    val X = r * 0.4124564d + g * 0.3575761d + b * 0.1804375d
    val Y = r * 0.2126729d + g * 0.7151522d + b * 0.0721750d
    val Z = r * 0.0193339d + g * 0.1191920d + b * 0.9503041d
    (X, Y, Z)
  }

  def xyz2lab(X: Double, Y: Double, Z: Double): (Double, Double, Double) = {
    val epsilon = 0.008856d
    val kappa = 903.3d

    val Xr = 0.950456d
    val Yr = 1d
    val Zr = 1.088754d

    val xr = X / Xr
    val yr = Y / Yr
    val zr = Z / Zr

    val fx = if (xr > epsilon) Math.pow(xr, 1d / 3d) else (kappa * xr + 16d) / 116d
    val fy = if (yr > epsilon) Math.pow(yr, 1d / 3d) else (kappa * yr + 16d) / 116d
    val fz = if (zr > epsilon) Math.pow(zr, 1d / 3d) else (kappa * zr + 16d) / 116d

    val L = 116d * fy - 16d
    val a = 500d * (fx - fy)
    val b = 200d * (fy - fz)

    (L, a, b)
  }

  def dist(c1: String, c2: String): Double = {
    val (r1, g1, b1) = hex2rgb(c1)
    val (r2, g2, b2) = hex2rgb(c2)
    val (x1, y1, z1) = rgb2xyz(r1 / 255d, g1 / 255d, b1 / 255d)
    val (x2, y2, z2) = rgb2xyz(r2 / 255d, g2 / 255d, b2 / 255d)
    val (labL1, laba1, labb1) = xyz2lab(x1, y1, z1)
    val (labL2, laba2, labb2) = xyz2lab(x2, y2, z2)
    distCIEDE2000(labL1, laba1, labb1, labL2, laba2, labb2)
  }

  def rndColor(min: Int, max: Int): String = {
    val rgb = (1 to 3).map(_ ⇒ min + Random.nextInt(max - min + 1)).toList
    val str = "#" + rgb.map("%02x".format(_)).mkString
    str
  }

  def rndHSLColor: String = {
    val h = Random.nextDouble()
    val s = 1d
    val l = 0.5d
    val rgb = hsl2rgb((h, s, l))
    rgb2hex(rgb)
  }

  def contrast(c: String): String = {
    val (h, s, v) = rgb2hsl(hex2rgb(c))
    val newh: Double = if (h > 0.5) h - 0.5 else h + 0.5
    val rgb = hsl2rgb((newh, s, v))
    rgb2hex(rgb)
  }

  def intensity(c: String, koeff: Double): String = {
    hex2rgb(c) match {
      case (r, g, b) ⇒
        def f(x: Int): Int = 255.min((x * koeff).toInt)
        rgb2hex(f(r), f(g), f(b))
    }
  }

  def intensities(c: String, koeffs: Double*): Array[String] = {
    val res = new Array[String](koeffs.size)
    hex2rgb(c) match {
      case (r, g, b) ⇒
        var idx = 0
        for (koeff ← koeffs) {
          @inline
          def f(x: Int): Int = 255.min((x * koeff).toInt)
          res(idx) = rgb2hex(f(r), f(g), f(b))
          idx += 1
        }
        res
    }
  }

  // todo: make it persistent with state of last queue
  def create(keep: Int = defaultKeep): Colors = new Impl(keep)

  /*
  * Concurrency model:
  *   when <= default, use wait free via CAS
  *   when > default, use synchronized
  * */
  class Impl(keep: Int) extends Colors {
    private val tries = 10

    private val last = MQueue[String](Default.list1000.takeRight(keep): _*)

    private val counter = new AtomicLong(-1L)

    private def generate(): String = this.synchronized {
      var i = 0
      var curColor: String = null
      var curDist = 0D
      while (i < tries) {
        val newColor = rndHSLColor
        val newDist = (last.iterator ++ banned.iterator).map(c ⇒ dist(c, newColor)).min
        if (newDist > curDist) {
          curDist = newDist
          curColor = newColor
        }
        i += 1
      }
      last.enqueue(curColor)
      while (last.size > keep) last.dequeue()
      curColor
    }

    def next(): String = {
      val id = counter.incrementAndGet()
      if (id < 1000) {
        Default.list1000(id.toInt)
      } else {
        generate()
      }
    }

  }

  val defaultKeep = 10

  val banned = Seq("#FFFF00", "#000000", "#FFFFFF")

  val selectColor = "#FFFF00"

  def toRGBA(color: String, a: Double) = {
    val (r, g, b) = hex2rgb(color)
    s"rgba($r,$g,$b,$a)"
  }

}
