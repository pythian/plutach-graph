package plutarch.server.data.metrics

import java.nio.{ ByteBuffer, ByteOrder }

import boopickle.Default.Pickle
import plutarch.server.data.accumulators.CombinedAccumulator
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.server.data.objects.Objects
import plutarch.shared.data.DataObject
import plutarch.server.data.raw.Raw
import plutarch.server.data.scale.Scale
import plutarch.server.data.store.MetricStoreCreator
import plutarch.shared.data.metrics.{ Conf, Meta }
import plutarch.shared.data.Picklers.ReqMeta

import scala.concurrent.{ ExecutionContext, Future }

trait Metric {
  def conf: Conf
  def meta: Meta
  def name: String
  def add(t: Long, values: Seq[(String, Double)])(implicit executor: ExecutionContext): Future[Unit]
  def get(aggregation: Aggregation, requestId: Int, scale: Int, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
  def get(aggregation: Aggregation, requestId: Int, scale: Int, intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[ByteBuffer]
  def getCurrent(aggregation: Aggregation)(scale: Int): ((Long, Long, Seq[(Int, Any)]), Seq[DataObject])
  def getCurrentKey: Long
}

// todo: possible bug: have to request objects wider than data to cover with step ?
object Metric {

  def create(conf: Conf, storeCreator: MetricStoreCreator): Metric = new Impl(conf, storeCreator)

  class Impl(val conf: Conf, storeCreator: MetricStoreCreator) extends Metric {
    import conf._

    private val accCreator = CombinedAccumulator.getInstanceCreator(aggregations)

    val rawStore: Raw = storeCreator.createRawStore()
    val scalesStore: Map[Int, Scale] = scales.map(scale ⇒ scale -> Scale.create(name, scale, step, accCreator, storeCreator, withTotal)).toMap
    val scalesStoreList: List[Scale] = scalesStore.values.toList
    val objectsStore: Objects = storeCreator.createObjectsStore()

    def name: String = conf.name

    def meta: Meta = Meta(conf, rawStore.current)

    def getObjects(names: TraversableOnce[String]): Seq[DataObject] = {
      names.toSet.iterator.map(objectsStore.getObject).toList
    }

    def add(t: Long, values: Seq[(String, Double)])(implicit executor: ExecutionContext): Future[Unit] = {
      rawStore.put(t, values)
      val valuesIds = values.map {
        case (objectName, value) ⇒
          val obj = objectsStore.check(t, objectName)
          (obj.id, value)
      }
      val futures = scalesStoreList.map(ss => ss.add(t, valuesIds))
      Future.sequence(futures).map(_ ⇒ ())
    }
    private def join(requestId: Int, aggregation: Aggregation, fobjs: Future[ByteBuffer], fdata: Future[ByteBuffer])(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      for {
        objs ← fobjs
        data ← fdata
      } yield {
        val meta = Pickle.intoBytes(new ReqMeta(requestId, aggregation))
        val metaLength = meta.limit() - meta.position()
        val objsLength = objs.limit() - objs.position()
        val dataLength = data.limit() - data.position()
        val totalLength = metaLength + objsLength + dataLength
        val buffer = ByteBuffer.allocateDirect(totalLength)
        buffer.put(meta)
        buffer.put(objs)
        buffer.put(data)
        buffer.flip()
        buffer.order(ByteOrder.LITTLE_ENDIAN)
        buffer
      }
    }
    def get(aggregation: Aggregation, requestId: Int, scale: Int, x: Long, y: Long)(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      val fobjs = objectsStore.get(x, y)
      val fdata = scalesStore(scale).get(aggregation, x, y)
      join(requestId, aggregation, fobjs, fdata)
    }
    private def join2(requestId: Int, aggregation: Aggregation, fobjs: Future[ByteBuffer], fdata: Future[Seq[ByteBuffer]])(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      for {
        objs ← fobjs
        datas ← fdata
      } yield {
        val meta = Pickle.intoBytes(new ReqMeta(requestId, aggregation))
        val metaLength = meta.limit() - meta.position()
        val objsLength = objs.limit() - objs.position()
        val dataLength = datas.map(data ⇒ data.limit() - data.position()).sum
        val totalLength = metaLength + objsLength + dataLength
        val buffer = ByteBuffer.allocateDirect(totalLength)
        buffer.put(meta)
        buffer.put(objs)
        datas.foreach(data ⇒ buffer.put(data))
        buffer.flip()
        buffer.order(ByteOrder.LITTLE_ENDIAN)
        buffer
      }
    }

    def get(aggregation: Aggregation, requestId: Int, scale: Int, intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[ByteBuffer] = {
      val fobjs = objectsStore.get(intervals)
      val fdata = scalesStore(scale).get(aggregation, intervals)
      join2(requestId, aggregation, fobjs, fdata)
    }

    def getCurrent(aggregation: Aggregation)(scale: Int): ((Long, Long, Seq[(Int, Any)]), Seq[DataObject]) = {
      val data = scalesStore(scale).getCurrent(aggregation)
      val objs = objectsStore.getDe(data._1 - scale * step, data._1 + scale * step)
      (data, objs)
    }

    def getCurrentKey: Long = rawStore.current

  }
}