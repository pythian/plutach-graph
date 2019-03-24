package plutarch.server.data.persistence

import java.nio.file.{ Files, Path, Paths }

import akka.actor.ActorSystem
import plutarch.server.data.objects.Objects
import plutarch.server.data.raw.Raw
import plutarch.server.data.scale.AggregationStore
import plutarch.server.data.store.{ MetricStoreCreator, MetricStoreCreatorCreator }
import plutarch.shared.data.Aggregations.Aggregation

/**
 * - create new
 * - open from saved: scan, if nothing create
 * - scan folder for metrics
 * - open metric from folder (recreate evrythign)
 *
 *
 * -baseDir
 *  |-metric1
 *  | |-raw
 *  | |-objects
 *  | |-aggregations
 *  |   |-step.agg
 *  |   | |-data.dat
 *  |   | |-offsets.dat
 *  |   |-...
 *  ...
 *  |-metricN
 *
 */

object FileManager {
  def apply(): FileManager = new FileManager
}

class FileManager {
  def scan(): Unit = {
    ???
  }
  def getCC(baseDir: Path)(implicit system: ActorSystem): FileMetricStoreCreatorCreator = {
    Files.createDirectories(baseDir)
    new FileMetricStoreCreatorCreator(baseDir)
  }
}

object FileMetricStoreCreatorCreator {
  def normalizeName(str: String): String = {
    str.filter(Character.isLetterOrDigit)
  }
}

class FileMetricStoreCreatorCreator(baseDir: Path)(implicit system: ActorSystem) extends MetricStoreCreatorCreator {
  import FileMetricStoreCreatorCreator._
  import system.dispatcher

  // open
  def create(conf: MetricStoreCreator.Conf): MetricStoreCreator = {
    val name = normalizeName(conf.name)
    val metricPath = baseDir.resolve(Paths.get(name))
    val rawStorePath = metricPath.resolve(Paths.get("raw"))
    val objectsStorePath = metricPath.resolve(Paths.get("objects"))
    val baseAggregationsStorePath = metricPath.resolve(Paths.get("aggregations"))

    new MetricStoreCreator {
      def createRawStore(): Raw = FileRawStore.open(rawStorePath)
      def createObjectsStore(): Objects = FileObjectsStore.open(objectsStorePath, 60000)
      def createAggregationStore(step: Long, aggregation: Aggregation): AggregationStore = {
        val name = s"$step.${aggregation.fname}"
        val aggregationsStorePath = baseAggregationsStorePath.resolve(Paths.get(name))
        FileAggregationStore.open(aggregationsStorePath)
      }
    }

  }

  // createNew
  def createNew(conf: MetricStoreCreator.Conf): MetricStoreCreator = {
    val name = normalizeName(conf.name)
    // todo: add store with metainf about metric, such as actual name

    val metricPath = baseDir.resolve(Paths.get(name))
    Files.createDirectory(metricPath)

    val rawStorePath = metricPath.resolve(Paths.get("raw"))
    val objectsStorePath = metricPath.resolve(Paths.get("objects"))
    val baseAggregationsStorePath = metricPath.resolve(Paths.get("aggregations"))

    Files.createDirectory(baseAggregationsStorePath)

    new MetricStoreCreator {
      def createRawStore(): Raw = FileRawStore.create(rawStorePath)
      def createObjectsStore(): Objects = FileObjectsStore.create(objectsStorePath, 60000)
      def createAggregationStore(step: Long, aggregation: Aggregation): AggregationStore = {
        val name = s"$step.${aggregation.fname}"
        val aggregationsStorePath = baseAggregationsStorePath.resolve(Paths.get(name))
        FileAggregationStore.create(aggregationsStorePath, step, 1 << 20) // todo partitions
      }
    }
  }
}
