package plutarch.server.pipelines.spark

import akka.actor.ActorSystem
import org.apache.spark.sql.{ DataFrame, Dataset, Row }
import org.apache.spark.sql.types.{ ArrayType, LongType, NumericType, StringType, StructType }
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.data.store.MetricStoreCreator
import plutarch.shared.data.metrics.{ Conf ⇒ MetricConf }
import scala.concurrent.{ Await, Future }

class SparkPipelineImpl(
    val pipelineConfig: SparkPipeline.Config,
    val metricConfig:   MetricConf,
    val storeConf:      MetricStoreCreator.Conf)(
    implicit
    system:        ActorSystem,
    metricManager: MetricManager) extends SparkPipeline {

  import system.dispatcher

  override val metric: Metric = metricManager.getOrCreate(metricConfig, storeConf)

  override def publish(t: Long, data: Seq[(String, Double)]): Future[Unit] = {
    metric.add(t, data)
  }

  def loadDataFrame(df: DataFrame): Unit = {
    import org.apache.spark.sql.functions.col
    if (df.schema.fields.length == 3) {
      // check if schema is (Timestamp, String, Double)
      val ts = df.schema.fields(0)
      val objectName = df.schema.fields(1)
      val value = df.schema.fields(2)
      if (ts.dataType.isInstanceOf[LongType] &&
        objectName.dataType.isInstanceOf[StringType] &&
        value.dataType.isInstanceOf[NumericType]) {
        df.sort(col(ts.name).asc).collect().iterator.grouped(pipelineConfig.batchSize).foreach { group: Seq[Row] ⇒
          val futures = group.map { row ⇒
            val t = row.getLong(0)
            val objectName = row.getString(1)
            val value = row.getAs[Number](2).doubleValue()
            publish(t, Seq((objectName, value)))
          }
          Await.result(Future.sequence(futures), pipelineConfig.timeout)
        }
        //metric.freeze()
        return
      }
    } else if (df.schema.length == 2) {
      // check if schema is (Timestamp, Seq(String, Double))
      val ts = df.schema.fields(0)
      val seq = df.schema.fields(1)
      if (ts.dataType.isInstanceOf[LongType] &&
        seq.dataType.isInstanceOf[ArrayType] &&
        seq.dataType.asInstanceOf[ArrayType].elementType.isInstanceOf[StructType]) {
        val rec = seq.dataType.asInstanceOf[ArrayType].elementType.asInstanceOf[StructType]
        if (rec.fields.length == 2) {
          val objectName = rec.fields(0)
          val value = rec.fields(1)
          if (objectName.dataType.isInstanceOf[StringType] &&
            value.dataType.isInstanceOf[NumericType]) {
            df.sort(col(ts.name).asc).collect().iterator.grouped(pipelineConfig.batchSize).foreach { group: Seq[Row] ⇒
              val futures = group.map { row ⇒
                val t = row.getLong(0)
                val values = row.getSeq[Row](1).map(r ⇒ (r.getString(0), r.getAs[Number](1).doubleValue()))
                publish(t, values)
              }
              Await.result(Future.sequence(futures), pipelineConfig.timeout)
            }
            //metric.freeze()
            return
          }
        }
      }
    }
    throw new RuntimeException("Can't recognize DataFrame format")
  }

  def loadDataSet[T <: SparkPipeline.TSGeneric](ds: Dataset[T]): Unit = {
    import org.apache.spark.sql.functions.col
    ds.sort(col("t").asc).collect().iterator.grouped(pipelineConfig.batchSize).foreach { group: Seq[T] ⇒
      val futures = group.map {
        case SparkPipeline.TSObjectRecord(t, objectName, value) ⇒
          publish(t, Seq((objectName, value)))
        case SparkPipeline.TSObjectsRecord(t, objectsValues) ⇒
          publish(t, objectsValues.map(_.toTuple))
        case SparkPipeline.TSObjectsTupleRecord(t, objectsValues) ⇒
          publish(t, objectsValues)
        case _ ⇒
          Future.successful()
      }
      Await.result(Future.sequence(futures), pipelineConfig.timeout)
    }
    //metric.freeze()
  }
}
