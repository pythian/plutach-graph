package plutarch.shared.data

import boopickle.Encoder
import plutarch.shared.data.metrics.Conf

import scala.util.{ Failure, Success, Try }

object Aggregations {

  sealed trait Aggregation {
    def displayName: String
    def name: String
    def fname: String
    def flag: Int
    def p: Double = 0.0
    override def toString: String = name
    def write(enc: Encoder, value: Any): Unit
    def getDouble(value: Any): Double
    def dataFormat(metric: Conf): Double ⇒ String = metric.dataFormat.apply
    def getUnit(metric: Conf): (Double, Int) ⇒ Double =
      (sizeY: Double, maxValues: Int) ⇒ metric.unitSelector(sizeY, maxValues)
  }
  object Min extends Aggregation {
    def displayName: String = "min"
    def name: String = "Min"
    val fname: String = "min"
    def flag: Int = 1
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object Max extends Aggregation {
    def displayName: String = "max"
    def name: String = "Max"
    val fname: String = "max"
    def flag: Int = 2
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object Sum extends Aggregation {
    def displayName: String = "sum"
    def name: String = "Sum"
    val fname: String = "sum"
    def flag: Int = 3
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object Count extends Aggregation {
    def displayName: String = "cnt"
    def name: String = "Count"
    val fname: String = "cnt"
    def flag: Int = 4
    def getDouble(value: Any): Double = value.asInstanceOf[Long]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeLong(value.asInstanceOf[Long])
    }
    override def dataFormat(metric: Conf): Double ⇒ String = _.toLong.toString
    override def getUnit(metric: Conf): (Double, Int) ⇒ Double =
      (sizeY: Double, maxValues: Int) ⇒ MetricDomain.DefaultUnitSelector(sizeY, maxValues)
  }
  object Stdev extends Aggregation {
    def displayName: String = "stdev"
    def name: String = "Stdev"
    val fname: String = "stdev"
    def flag: Int = 5
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
    override def dataFormat(metric: Conf): Double ⇒ String = MetricDomain.toStr
    override def getUnit(metric: Conf): (Double, Int) ⇒ Double =
      (sizeY: Double, maxValues: Int) ⇒ MetricDomain.DefaultUnitSelector(sizeY, maxValues)
  }
  object CountDistinct extends Aggregation {
    def displayName: String = "cntdst"
    def name: String = "CountDistinct"
    val fname: String = "cntdst"
    def flag: Int = 6
    def getDouble(value: Any): Double = value.asInstanceOf[Int]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeInt(value.asInstanceOf[Int])
    }
    override def dataFormat(metric: Conf): Double ⇒ String = _.toInt.toString
    override def getUnit(metric: Conf): (Double, Int) ⇒ Double =
      (sizeY: Double, maxValues: Int) ⇒ MetricDomain.DefaultUnitSelector(sizeY, maxValues)
  }
  object Mean extends Aggregation {
    def displayName: String = "mean"
    def name: String = "Mean"
    val fname: String = "mean"
    def flag: Int = 7
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object First extends Aggregation {
    def displayName: String = "first"
    def name: String = "First"
    val fname: String = "first"
    def flag: Int = 8
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object Last extends Aggregation {
    def displayName: String = "last"
    def name: String = "Last"
    val fname: String = "last"
    def flag: Int = 9
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object PercentileCont {
    val fname: String = "pctc"
  }
  case class PercentileCont(override val p: Double) extends Aggregation {
    def displayName: String = s"pctc_${(p * 100).round}"
    def name: String = s"PercentileCont($p)"
    def fname: String = s"pctc.${(p * 100).round}"
    def flag: Int = 10
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object PercentileDesc {
    val fname: String = "pctd"
  }
  case class PercentileDesc(override val p: Double) extends Aggregation {
    def displayName: String = s"pctd_${(p * 100).round}"
    def name: String = s"PercentileDesc($p)"
    def fname: String = s"pctd.${(p * 100).round}"
    def flag: Int = 11
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }
  object Avg extends Aggregation {
    def displayName: String = "avg"
    def name: String = "Avg"
    val fname: String = "avg"
    def flag: Int = 12
    def getDouble(value: Any): Double = value.asInstanceOf[Double]
    def write(enc: Encoder, value: Any): Unit = {
      enc.writeDouble(value.asInstanceOf[Double])
    }
  }

  def flagToAggregation(flag: Int, p: Double): Aggregation = flag match {
    case 1  ⇒ Min
    case 2  ⇒ Max
    case 3  ⇒ Sum
    case 4  ⇒ Count
    case 5  ⇒ Stdev
    case 6  ⇒ CountDistinct
    case 7  ⇒ Mean
    case 8  ⇒ First
    case 9  ⇒ Last
    case 10 ⇒ PercentileCont(p)
    case 11 ⇒ PercentileDesc(p)
    case 12 ⇒ Avg
  }

  private object StrDotInt {
    def unapply(str: String): Option[(String, Int)] = {
      val parts = str.split('.')
      if (parts.length == 2) {
        val pstr = parts(1)
        Try(pstr.toInt) match {
          case Success(p) ⇒ Some((parts(0), p))
          case Failure(_) ⇒ None
        }
      } else {
        None
      }
    }
  }

  def apply(aggregation: Aggregation): String = aggregation.fname
  def unapply(str: String): Option[Aggregation] = str match {
    case Min.fname                          ⇒ Some(Min)
    case Max.fname                          ⇒ Some(Max)
    case Sum.fname                          ⇒ Some(Sum)
    case Count.fname                        ⇒ Some(Count)
    case Stdev.fname                        ⇒ Some(Stdev)
    case CountDistinct.fname                ⇒ Some(CountDistinct)
    case Mean.fname                         ⇒ Some(Mean)
    case First.fname                        ⇒ Some(First)
    case Last.fname                         ⇒ Some(Last)
    case Avg.fname                          ⇒ Some(Avg)
    case StrDotInt(PercentileCont.fname, p) ⇒ Some(PercentileCont(p / 100))
    case StrDotInt(PercentileDesc.fname, p) ⇒ Some(PercentileDesc(p / 100))
  }

  val allFlags: Seq[Int] = 1 to 12

}
