package plutarch.shared.data

import java.nio.ByteBuffer

import boopickle.{ Decoder, DecoderSize, DecoderSpeed, PickleState, PicklerHelper, UnpickleState }
import boopickle.Default._
import plutarch.shared.data.Aggregations.Aggregation

object Picklers extends PicklerHelper {

  class ReqMeta(val requestId: Int, val aggregation: Aggregation)

  class AggData(val aggregation: Aggregation, val key: Long, val size: Int, val data: Iterator[(Int, Any)])

  implicit object AggDataPickler extends P[AggData] {
    override def pickle(obj: AggData)(implicit state: PickleState): Unit = {
      state.enc.writeLong(obj.key)
      state.enc.writeInt(obj.size)
      obj.data.foreach {
        case (id, value) ⇒
          state.enc.writeInt(id)
          obj.aggregation.write(state.enc, value)
      }
    }
    override def unpickle(implicit state: UnpickleState): AggData = {
      throw new Exception("Operation is not supported!")
    }
  }

  type TypedMetric[T] = Iterator[(Long, Iterator[(Int, T)])]
  type DoubleMetric = TypedMetric[Double]
  type LongMetric = TypedMetric[Long]
  type IntMetric = TypedMetric[Int]

  def getJson[T](data: TypedMetric[T]): String = {
    data
      .map(x ⇒ s""""${x._1}": ${x._2.map(y ⇒ s""""${y._1}": ${y._2}""").mkString("{", ", ", "}")}""")
      .mkString("{", ", ", "}")
  }

  implicit object DoubleMetricPickler extends TypedMetricPicker[Double](_.readDouble)
  implicit object LongMetricPickler extends TypedMetricPicker[Long](_.readLong)
  implicit object IntMetricPickler extends TypedMetricPicker[Int](_.readInt)

  //use order(ByteOrder.LITTLE_ENDIAN) for those are passed here!
  abstract class TypedMetricPicker[T](getDe: Decoder ⇒ T) extends P[TypedMetric[T]] {
    override def pickle(obj: TypedMetric[T])(implicit state: PickleState): Unit = {
      throw new Exception("Operation is not supported!")
    }
    override def unpickle(implicit state: UnpickleState): TypedMetric[T] = {
      val buf = state.dec match {
        case dec: DecoderSize  ⇒ dec.buf
        case dec: DecoderSpeed ⇒ dec.buf
      }
      val iterator = new TypedMetric[T] {
        def hasNext: Boolean = buf.hasRemaining
        def next: (Long, Iterator[(Int, T)]) = {
          val tmsp = state.dec.readLong
          val objCount = state.dec.readInt
          val objIterator: Iterator[(Int, T)] = new Iterator[(Int, T)] {
            private var objIdx = 0
            def hasNext: Boolean = objIdx < objCount
            def next: (Int, T) = {
              val obj = state.dec.readInt
              val value = getDe(state.dec)
              objIdx += 1
              obj -> value
            }
          }
          (tmsp, objIterator)
        }
      }
      iterator
    }
  }

  implicit object MetaPickler extends P[ReqMeta] {
    override def pickle(obj: ReqMeta)(implicit state: PickleState): Unit = {
      state.enc.writeInt(obj.requestId)
      state.enc.writeInt(obj.aggregation.flag)
      state.enc.writeDouble(obj.aggregation.p)
    }
    override def unpickle(implicit state: UnpickleState): ReqMeta = {
      val requestId = state.dec.readInt
      val flagAggregation = state.dec.readInt
      val p = state.dec.readDouble
      new ReqMeta(requestId, Aggregations.flagToAggregation(flagAggregation, p))
    }
  }

  // combined
  sealed trait CombinedData[T] {
    def requestId: Int
    def objects: Seq[DataObject]
    def aggregation: Aggregation
    def data: TypedMetric[T]
    override def toString: String =
      s"""{"requestId": $requestId, "objects": ${objects.mkString("[", ", ", "]")}, "aggregation": "$aggregation", "data": ${getJson(data)}}"""

    def dummy: Int = {
      var cnt = 0
      data.foreach { x ⇒
        x._2.foreach { y ⇒
          cnt += 1
        }
      }
      cnt
    }
  }
  case class CombinedIntData(requestId: Int, objects: Seq[DataObject], aggregation: Aggregation, data: TypedMetric[Int]) extends CombinedData[Int]
  case class CombinedLongData(requestId: Int, objects: Seq[DataObject], aggregation: Aggregation, data: TypedMetric[Long]) extends CombinedData[Long]
  case class CombinedDoubleData(requestId: Int, objects: Seq[DataObject], aggregation: Aggregation, data: TypedMetric[Double]) extends CombinedData[Double]

  def extractCombinedData(agg: Aggregation, buffer: ByteBuffer): TypedMetric[Any] = {
    import Aggregations._
    agg match {
      case Min | Max | Sum | Stdev | Mean | First | Last | PercentileCont(_) | PercentileDesc(_) | Avg ⇒
        Unpickle[DoubleMetric].fromBytes(buffer)
      case Count ⇒
        Unpickle[LongMetric].fromBytes(buffer)
      case CountDistinct ⇒
        Unpickle[IntMetric].fromBytes(buffer)
    }
  }

  def extract(buffer: ByteBuffer): CombinedData[_] = {
    val meta = Unpickle[ReqMeta].fromBytes(buffer)
    val objects = Unpickle[Seq[DataObject]].fromBytes(buffer)
    import Aggregations._
    meta.aggregation match {
      case Min | Max | Sum | Stdev | Mean | First | Last | PercentileCont(_) | PercentileDesc(_) | Avg ⇒
        val data = Unpickle[DoubleMetric].fromBytes(buffer)
        CombinedDoubleData(meta.requestId, objects, meta.aggregation, data)
      case Count ⇒
        val data = Unpickle[LongMetric].fromBytes(buffer)
        CombinedLongData(meta.requestId, objects, meta.aggregation, data)
      case CountDistinct ⇒
        val data = Unpickle[IntMetric].fromBytes(buffer)
        CombinedIntData(meta.requestId, objects, meta.aggregation, data)
    }
  }

}