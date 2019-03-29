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

package plutarch.server.data.metrics

import java.nio.ByteOrder

import org.scalatest.{ FunSuite, Matchers }
import plutarch.shared.data.{ Aggregations, DataObject, Picklers }
import plutarch.server.data.objects.Objects
import plutarch.shared.data.metrics.Conf

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.util.Random
import scala.concurrent.duration._
import boopickle.Default._
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.scale.Scale
import plutarch.server.data.store.DefaultMetricStoreCreatorCreator
import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.Picklers._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

object ImplicitDeserialized {

  //  def getDe(aggregation: Aggregation, x: Long, y: Long)(implicit executor: ExecutionContext): Future[Seq[(Long, Seq[(Int, Any)])]]
  //  def getAllDe(x: Long, y: Long)(implicit executor: ExecutionContext): Future[Seq[(Long, Seq[(Int, IndexedSeq[Any])])]]
  //    //    def getDe(aggregation: Aggregation, scale: Int, x: Long, y: Long): (Seq[(Long, Seq[(Int, Any)])], Seq[DataObject]) = {
  //    //      val data = scalesStore(scale).getDe(aggregation, x, y)
  //    //      val objs = objectsStore.getDe(x, y)
  //    //      (data, objs)
  //    //    }
  //    //
  //    //    def getAllDe(scale: Int, x: Long, y: Long): (Seq[(Long, Seq[(Int, Seq[Any])])], Seq[DataObject]) = {
  //    //      val data = scalesStore(scale).getAllDe(x, y)
  //    //      val objs = objectsStore.getDe(x, y)
  //    //      (data, objs)
  //    //    }

  implicit class RichScale(scale: Scale.Impl) {
    def getDe(aggregation: Aggregation, x: Long, y: Long): Future[Seq[(Long, Seq[(Int, Any)])]] = for {
      buffer ← scale.aggregationsStore.get(aggregation, x, y)
    } yield {
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      val data = Picklers.extractCombinedData(aggregation, buffer)
      val lb1 = ListBuffer.empty[(Long, Seq[(Int, Any)])]
      for (elem1 ← data) {
        val lb2 = ListBuffer.empty[(Int, Any)]
        for (elem2 ← elem1._2) {
          lb2 += (elem2._1 -> elem2._2)
        }
        lb1 += (elem1._1 -> lb2.result())
      }
      lb1.result()
    }

    // method not for use really, just for testing!
    def getAllDe(x: Long, y: Long): Future[Seq[(Long, Seq[(Int, IndexedSeq[Any])])]] = for {
      data ← Future.sequence(scale.accCreator.getAggregations.map(agg ⇒ getDe(agg, x, y)).toIndexedSeq)
    } yield {
      val res = ListBuffer.empty[(Long, Seq[(Int, IndexedSeq[Any])])]
      val itetators = data.map(_.iterator)
      val head = itetators.head
      while (head.hasNext) {
        val data = itetators.map(_.next())
        val key = data.head._1
        val objValues = data.map(_._2).zipWithIndex.flatMap {
          case (objData, idx) ⇒
            objData.map(obj ⇒ (obj._1, idx, obj._2))
        }.groupBy(_._1).map {
          case (objId, values) ⇒
            objId -> values.map(_._3)
        }.toSeq
        res += (key -> objValues)
      }
      res.result()
    }
  }

  implicit class RichMetric(metric: Metric) {
    def getAllDe(scale: Int, x: Long, y: Long): (Seq[(Long, Seq[(Int, Seq[Any])])], Seq[DataObject]) = metric match {
      case metric: Metric.Impl ⇒
        val data = metric.scalesStore(scale) match {
          case scale: Scale.Impl ⇒
            scale.getAllDe(x, y)
          case _ ⇒
            throw new Exception("Operation is not supported")
        }
        val objs = metric.objectsStore.getDe(x, y)
        (Await.result(data, 10 second), objs)
      case _ ⇒
        throw new Exception("Operation is not supported")
    }
  }
}

class MetricTest extends FunSuite with Matchers {
  import ImplicitDeserialized._

  def await[T](fut: Future[T]): T = {
    Await.result(fut, 10 second)
  }

  test("emptyTest") {

    val conf = Conf(
      scales = Seq(1, 10, 100),
      withTotal = true,
      name = "empty",
      step = 1000,
      aggregations = Seq(Aggregations.Sum))

    val metrics: Metric = MetricManager.create(DefaultMetricStoreCreatorCreator).getOrCreate(conf)

    metrics.getAllDe(1, Long.MinValue, Long.MaxValue) shouldBe (Seq(), Seq(DataObject(0, "total", "#43CD80")))

    val megabuffer = await(metrics.get(Aggregations.Sum, 0, 1, Long.MinValue, Long.MaxValue))
    megabuffer.order() shouldBe ByteOrder.LITTLE_ENDIAN
    val agg_1_1 = Unpickle[ReqMeta].fromBytes(megabuffer).aggregation
    val objs1_1 = Unpickle[Seq[DataObject]].fromBytes(megabuffer)
    val data1_1 = Unpickle[DoubleMetric].fromBytes(megabuffer)
    objs1_1 shouldBe Seq(DataObject(0, "total", "#43CD80"))
    agg_1_1 shouldBe Aggregations.Sum
    // toList order matters a lot!!! as there are inlined iterators which must be read in strict order
    data1_1.map(x ⇒ (x._1, x._2.toList.sortBy(_._1))).toList shouldBe empty

    // extract check
    val megabuffer2 = await(metrics.get(Aggregations.Sum, 0, 1, Long.MinValue, Long.MaxValue))
    val combibed = Picklers.extract(megabuffer2)
    combibed.objects shouldBe Seq(DataObject(0, "total", "#43CD80"))
    combibed.aggregation shouldBe Aggregations.Sum
    combibed.data.map(x ⇒ (x._1, x._2.toList.sortBy(_._1))).toList shouldBe empty
  }

  test("simple") {

    val conf = Conf(
      scales = Seq(1, 10, 100),
      withTotal = true,
      name = "simple",
      step = 1000,
      aggregations = Seq(Aggregations.Sum))

    val metrics: Metric = MetricManager.create(DefaultMetricStoreCreatorCreator).getOrCreate(conf)

    metrics.add(0, Seq(("obj0", 1d)))
    metrics.add(1000, Seq(("obj0", 2d)))
    metrics.add(2001, Seq(("obj0", 3d)))
    metrics.add(3999, Seq(("obj0", 4d)))
    metrics.add(4001, Seq(("obj0", 5d)))
    metrics.add(5999, Seq(("obj0", 6d)))
    metrics.add(6001, Seq(("obj0", 7d)))
    metrics.add(7999, Seq(("obj0", 8d)))
    metrics.add(8001, Seq(("obj0", 9d)))
    metrics.add(9999, Seq(("obj0", 10d)))
    metrics.add(10000, Seq())

    val (data1, objs1) = metrics.getAllDe(1, 0, 10000)
    val expected1 = (0 to 9).map(i ⇒ (i * 1000, Seq((1, Seq(1d + i)), (0, Seq(1d + i)))))
    data1 shouldBe expected1
    objs1.map(_.name).filterNot(_ == "total") shouldBe Seq("obj0")

    val (data2, objs2) = metrics.getAllDe(10, 0, 11000)
    val expected2 = Seq((0, Seq((1, Seq(55d)), (0, Seq(55d)))))
    data2 shouldBe expected2
    objs2.map(_.name).filterNot(_ == "total") shouldBe Seq("obj0")

  }

  test("real simulation") {

    val scalesX = Seq(2, 5, 10, 50, 100)

    val conf = Conf(
      scales = 1 +: scalesX,
      withTotal = true,
      name = "real",
      step = 1000,
      aggregations = Seq(Aggregations.Sum, Aggregations.Max, Aggregations.Count))

    val metrics: Metric = MetricManager.create(DefaultMetricStoreCreatorCreator).getOrCreate(conf)
    val objects = Objects.create("temp")

    val t0 = 0
    val t1 = 1000000
    val maxtime = 100000
    val maxValue = 100
    val count = 1000

    // objects life intervals
    Random.setSeed(0)
    val objs = for (i ← 1 to count) yield {
      val objName = s"obj$i"
      val objFirst = t0 + (Random.nextInt(t1 - t0) / conf.step) * conf.step
      val objLast = objFirst + (Random.nextInt(maxtime) / conf.step) * conf.step
      (objFirst, objLast, objName)
    }

    // object values on those intervals
    val objsValues: Seq[(Int, Seq[(String, Double)])] = for (t ← t0.to(t1, conf.step.toInt)) yield {
      val thisObjs = objs.filter(obj ⇒ (obj._1 <= t && t <= obj._2) && (obj._1 == t || obj._2 == t || Random.nextDouble < 0.6))
      (t, thisObjs.map(obj ⇒ (obj._3, Random.nextInt(maxValue).toDouble)))
    }
    val objsValuesMap = objsValues.toMap

    // load data
    objsValues.foreach {
      case (time, data) ⇒
        metrics.add(time, data)
        data.foreach { x ⇒
          objects.check(time, x._1)
        }
    }

    // interval of interest
    val left = (t0 + t1) / 2
    val right = left + maxtime

    // calculate expected result from raw data for scale 1
    def calcScale1(from: Int, until: Int): Seq[(Long, Seq[(Int, Seq[Any])])] = {
      from.until(until, conf.step.toInt).map { t ⇒
        val values = objsValuesMap(t)
        val sum = values.map(_._2).sum
        val max = values.map(_._2).max
        val cnt = values.size
        val total = (0, Seq[Any](sum, max, cnt))
        val others = values.map(x ⇒ (objects.check(t, x._1).id, Seq[Any](x._2, x._2, 1))).sortBy(_._1)
        (t.toLong, total +: others)
      }
    }

    // calculate expected result from raw data for scale > 1
    def calcScaleN(scale: Int): Seq[(Long, Seq[(Int, Seq[Any])])] = {
      calcScale1(left, right)
        .map(x ⇒ ((x._1 / scale / conf.step) * scale * conf.step, x._2))
        .groupBy(_._1)
        .mapValues {
          lst: Seq[(Long, Seq[(Int, Seq[Any])])] ⇒
            val res: Seq[(Int, Seq[Any])] = lst.flatMap(_._2).groupBy(_._1).mapValues { vals: Seq[(Int, Seq[Any])] ⇒
              val (sum, max, cnt) =
                vals.map(_._2)
                  .map(x ⇒ (
                    x(0).asInstanceOf[Double],
                    x(1).asInstanceOf[Double],
                    x(2).asInstanceOf[Int])).unzip3
              Seq[Any](sum.sum, max.max, cnt.sum)
            }.toSeq.sortBy(_._1)
            res
        }.toSeq.sortBy(_._1)
    }

    val (data1, objs1) = metrics.getAllDe(1, left, right)
    val expectedObjs = objs.filter(obj ⇒ obj._1 <= right && obj._2 >= left).map(_._3).toSet
    objects.getDe(left, right).map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expectedObjs
    objs1.map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expectedObjs
    val expectedData1: Seq[(Long, Seq[(Int, Seq[Any])])] = calcScale1(left, right)
    data1.map(x ⇒ (x._1, x._2.sortBy(_._1))) shouldBe expectedData1

    val megabuffer = await(metrics.get(Aggregations.Sum, 0, 1, left, right))
    megabuffer.order() shouldBe ByteOrder.LITTLE_ENDIAN
    val agg_1_1 = Unpickle[ReqMeta].fromBytes(megabuffer).aggregation
    val objs1_1 = Unpickle[Seq[DataObject]].fromBytes(megabuffer)
    val data1_1 = Unpickle[DoubleMetric].fromBytes(megabuffer)
    objs1_1.map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expectedObjs
    agg_1_1 shouldBe Aggregations.Sum
    // toList order matters a lot!!!
    data1_1.map(x ⇒ (x._1, x._2.toList.sortBy(_._1))).toList shouldBe expectedData1.map(x ⇒ (x._1, x._2.map(x ⇒ (x._1, x._2.head))))

    def testScale(scale: Int): Unit = {
      val (data2, objs2) = metrics.getAllDe(scale, left, right)
      objs2.map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expectedObjs
      val expectedData2: Seq[(Long, Seq[(Int, Seq[Any])])] = calcScaleN(scale)
      data2.map(x ⇒ (x._1, x._2.sortBy(_._1))) shouldBe expectedData2

      val megabuffer = await(metrics.get(Aggregations.Sum, 0, scale, left, right))
      megabuffer.order() shouldBe ByteOrder.LITTLE_ENDIAN
      val agg_2_1 = Unpickle[ReqMeta].fromBytes(megabuffer).aggregation
      val objs2_1 = Unpickle[Seq[DataObject]].fromBytes(megabuffer)
      val data2_1 = Unpickle[DoubleMetric].fromBytes(megabuffer)
      objs2_1.map(_.name).filterNot(_ == "total").toSet should contain allElementsOf expectedObjs
      agg_2_1 shouldBe Aggregations.Sum
      // toList order matters a lot!!!
      data2_1.map(x ⇒ (x._1, x._2.toList.sortBy(_._1))).toList shouldBe expectedData2.map(x ⇒ (x._1, x._2.map(x ⇒ (x._1, x._2.head))))

      println(s"tested scale=$scale, size=${expectedData2.size}")
    }

    scalesX.foreach(testScale)

    def testTotal(): Unit = {

      val totalExpectedObjs = objs.map(_._3).toSet
      val totalExpectedData = calcScale1(t0, t1)

      val megabuffer = await(metrics.get(Aggregations.Sum, 0, 1, Long.MinValue, Long.MaxValue))
      megabuffer.order() shouldBe ByteOrder.LITTLE_ENDIAN
      val agg_3_1 = Unpickle[ReqMeta].fromBytes(megabuffer).aggregation
      val objs3_1 = Unpickle[Seq[DataObject]].fromBytes(megabuffer)
      val data3_1 = Unpickle[DoubleMetric].fromBytes(megabuffer)
      objs3_1.map(_.name).filterNot(_ == "total").toSet should contain allElementsOf totalExpectedObjs
      agg_3_1 shouldBe Aggregations.Sum
      // toList order matters a lot!!!
      data3_1.map(x ⇒ (x._1, x._2.toList.sortBy(_._1))).toList shouldBe totalExpectedData.map(x ⇒ (x._1, x._2.map(x ⇒ (x._1, x._2.head))))
    }

    testTotal()
  }

}
