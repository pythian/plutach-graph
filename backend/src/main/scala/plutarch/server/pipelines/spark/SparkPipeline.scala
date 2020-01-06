package plutarch.server.pipelines.spark

import org.apache.spark.sql.{ DataFrame, Dataset }
import plutarch.server.data.metrics.Metric
import scala.concurrent.Future
import scala.concurrent.duration._

object SparkPipeline {

  val BATCH_SIZE = 100
  val TIMEOUT = 10 seconds

  case class Config(batchSize: Int = BATCH_SIZE, timeout: Duration = TIMEOUT)

  sealed trait TSGeneric {
    def t: Long
  }
  case class TSObjectRecord(t: Long, objectName: String, value: Double) extends TSGeneric
  case class ObjectValue(objectName: String, value: Double) {
    def toTuple: (String, Double) = (objectName, value)
  }
  case class TSObjectsRecord(t: Long, objectsValues: Seq[ObjectValue]) extends TSGeneric
  case class TSObjectsTupleRecord(t: Long, objectsValues: Seq[(String, Double)]) extends TSGeneric
}

trait SparkPipeline {
  def pipelineConfig: SparkPipeline.Config
  def metric: Metric
  def publish(t: Long, data: Seq[(String, Double)]): Future[Unit]
  def loadDataSet[T <: SparkPipeline.TSGeneric](ds: Dataset[T]): Unit
  def loadDataFrame(df: DataFrame): Unit
}
