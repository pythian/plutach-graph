package plutarch.client.experemental

import scala.scalajs.js.Date
import scala.concurrent.duration._
import plutarch.client.experemental.Implicits._

object DateTools {
  def l2(i: Int): String = (if (i < 10) "0" else "") + i.toString
  def l3(i: Int): String = (if (i < 10) "00" else if (i < 100) "0" else "0") + i.toString

  val TZ_OFFSET: Long = (new Date).getTimezoneOffset() * 60000
  val UTC_MONDAY: Long = 345600000l
  val LOCAL_MONDAY: Long = UTC_MONDAY + TZ_OFFSET
  val SEVEN_DAYS: Long = 7.days.toMillis
  val ONE_DAY: Long = 1.day.toMillis

  def thisYear(date: Date): Date = new Date(date.getFullYear(), 0, 1)
  def thisYear(x: Long): Long = thisYear(new Date(x)).getTime().toLong
  def thisQuarter(date: Date): Date = new Date(date.getFullYear(), (date.getMonth() / 3) * 3, 1)
  def thisQuarter(x: Long): Long = thisQuarter(new Date(x)).getTime().toLong
  def thisMonth(date: Date): Date = new Date(date.getFullYear(), date.getMonth(), 1)
  def thisMonth(x: Long): Long = thisMonth(new Date(x)).getTime().toLong

  def next10Year(date: Date): Date = new Date((date.getFullYear() / 10) * 10 + 10, 0, 1)
  def next10Year(x: Long): Long = next10Year(new Date(x)).getTime().toLong
  def nextYear(date: Date): Date = new Date(date.getFullYear() + 1, 0, 1)
  def nextYear(x: Long): Long = nextYear(new Date(x)).getTime().toLong
  def nextQuarter(date: Date): Date = new Date(date.getFullYear(), (date.getMonth() / 3) * 3 + 3, 1)
  def nextQuarter(x: Long): Long = nextQuarter(new Date(x)).getTime().toLong
  def nextMonth(date: Date): Date = new Date(date.getFullYear(), date.getMonth() + 1, 1)
  def nextMonth(x: Long): Long = nextMonth(new Date(x)).getTime().toLong

  def yearAgo(x: Long, k: Int = 1): Long = {
    val date = new Date(x)
    new Date(
      date.getFullYear() - k,
      date.getMonth(),
      date.getDate(),
      date.getHours(),
      date.getMinutes(),
      date.getSeconds(),
      date.getMilliseconds()).getTime().toLong
  }

  def monthAgo(x: Long): Long = {
    val date = new Date(x)
    new Date(
      date.getFullYear(),
      date.getMonth() - 1,
      date.getDate(),
      date.getHours(),
      date.getMinutes(),
      date.getSeconds(),
      date.getMilliseconds()).getTime().toLong
  }

  def toDateStr(d: Double, withDate: Boolean = true, withTime: Boolean = true, withSeconds: Boolean = true, withMillis: Boolean = true): String = {
    val dt = new Date(d)
    def date = s"${dt.getFullYear}-${l2(dt.getMonth + 1)}-${l2(dt.getDate)}"
    def time = s"${l2(dt.getHours)}:${l2(dt.getMinutes)}"
    def seconds = s"${l2(dt.getSeconds)}"
    def millis = dt.getMilliseconds
    if (withDate && withTime && withSeconds && withMillis) {
      s"$date $time:$seconds.${l3(millis)}"
    } else if (withDate && withTime && withSeconds && !withMillis) {
      s"$date $time:$seconds"
    } else if (withDate && withTime && !withSeconds && !withMillis) {
      s"$date $time"
    } else if (withDate && !withTime && !withMillis) {
      date
    } else if (!withDate && withTime && withSeconds && withMillis) {
      s"$time:$seconds.${l3(millis)}"
    } else if (!withDate && withTime && withSeconds && !withMillis) {
      s"$time:$seconds"
    } else if (!withDate && withTime && !withSeconds && !withMillis) {
      s"$time"
    } else if (!withDate && !withTime && withMillis) {
      s"$millis"
    } else {
      "incorrect with- combination"
    }
    //s"$year-$mounth-$date $hours:$minutes:$seconds.$millis"
  }

  def toDateStr(d: Double, u: Double): String = {
    timeIntervals.find(_._1 == u) match {
      case Some((_, _, units)) ⇒
        val withMillis = units == 1
        val withSeconds = units == 2
        val withMinutes = units == 3
        val withHours = units == 4
        toDateStr(
          d,
          //withDate = true,
          withTime = withHours || withMinutes || withSeconds || withMillis,
          withSeconds = withSeconds || withMillis,
          withMillis = withMillis)
      case _ ⇒ toDateStr(d)
    }
  }

  private val timeIntervals = List(
    (1.millis.toMillis.toDouble, "1ms", 1),
    (10.millis.toMillis.toDouble, "10ms", 1),
    (100.millis.toMillis.toDouble, "100ms", 1),
    (1.second.toMillis.toDouble, "1s", 2),
    (2.second.toMillis.toDouble, "2s", 2),
    (5.second.toMillis.toDouble, "5s", 2),
    (10.second.toMillis.toDouble, "10s", 2),
    (30.second.toMillis.toDouble, "30s", 2),
    (1.minute.toMillis.toDouble, "1m", 3),
    (2.minute.toMillis.toDouble, "2m", 3),
    (5.minute.toMillis.toDouble, "5m", 3),
    (10.minute.toMillis.toDouble, "10m", 3),
    (30.minute.toMillis.toDouble, "30m", 3),
    (1.hour.toMillis.toDouble, "1h", 4),
    (2.hour.toMillis.toDouble, "2h", 4),
    (6.hour.toMillis.toDouble, "6h", 4),
    (12.hour.toMillis.toDouble, "12h", 4),
    (1.day.toMillis.toDouble, "1d", 5),
    (2.day.toMillis.toDouble, "2d", 5),
    (5.day.toMillis.toDouble, "5d", 5),
    (10.day.toMillis.toDouble, "10d", 5),
    (20.day.toMillis.toDouble, "20d", 5),
    (30.day.toMillis.toDouble, "30d", 5),
    (50.day.toMillis.toDouble, "50d", 5),
    (100.day.toMillis.toDouble, "100d", 5),
    (1000.day.toMillis.toDouble, "1000d", 5),
    (10000.day.toMillis.toDouble, "10000d", 5))
  def msIntervalToUnit(ms: Double, maxcnt: Double): Double =
    timeIntervals.map(u ⇒ (ms / u._1, u._1)).filter(_._1 < maxcnt).map(_._2).optionMin.getOrElse(ms)

  def toTimeStr(d: Double, u: Double): String = {
    timeIntervals.find(_._1 == u) match {
      case Some((_, _, units)) ⇒
        val withMillis = units == 1
        val withSeconds = units == 2
        val withMinutes = units == 3
        val withHours = units == 4
        val withDays = units == 5
        toTimeStr(d, withMillis, withSeconds, withMinutes, withHours, withDays)
      case _ ⇒ toTimeStr(d)
    }
  }
  def toTimeStrAuto(d: Double): String =
    toTimeStr(d, timeIntervals.reverse.find(_._1 <= d).map(_._1).getOrElse(1d))

  def toTimeStr(d: Double, withMillis: Boolean = false, withSeconds: Boolean = false, withMinutes: Boolean = false, withHours: Boolean = false, withDays: Boolean = false): String = {
    val ad = d.abs
    val millis = (ad % 1000).toInt
    val seconds = ((ad / 1000) % 60).toInt
    val minutes = ((ad / 1000 / 60) % 60).toInt
    val hours = ((ad / 1000 / 60 / 60) % 24).toInt
    val days = (ad / 1000 / 60 / 60 / 24).toInt
    if (days > 0) {
      if (millis > 0 || withMillis) {
        s"${days}d ${hours}h ${minutes}m $seconds.${l3(millis)}s"
      } else if (seconds > 0 || withSeconds) {
        s"${days}d ${hours}h ${minutes}m ${seconds}s"
      } else if (minutes > 0 || withMinutes) {
        s"${days}d ${hours}h ${minutes}m"
      } else if (hours > 0 || withHours) {
        s"${days}d ${hours}h"
      } else {
        s"${days}d"
      }
    } else if (hours > 0) {
      if (millis > 0 || withMillis) {
        s"${hours}h ${minutes}m $seconds.${l3(millis)}s"
      } else if (seconds > 0 || withSeconds) {
        s"${hours}h ${minutes}m ${seconds}s"
      } else if (minutes > 0 || withMinutes) {
        s"${hours}h ${minutes}m"
      } else {
        s"${hours}h"
      }
    } else if (minutes > 0) {
      if (millis > 0 || withMillis) {
        s"${minutes}m $seconds.${l3(millis)}s"
      } else if (seconds > 0 || withSeconds) {
        s"${minutes}m ${seconds}s"
      } else {
        s"${minutes}m"
      }
    } else if (seconds > 0) {
      if (millis > 0 || withMillis) {
        s"$seconds.${l3(millis)}s"
      } else {
        s"${seconds}s"
      }
    } else if (millis > 0) {
      s"0.${millis}s"
    } else {
      "0"
    }
  }
}
