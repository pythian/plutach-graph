package plutarch.server.data.metricManager

import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metrics.Metric
import plutarch.server.data.store._
import plutarch.shared.data.metrics.Conf

/*
*
* Persist state
* load metrics from disk and other tasks
*
* */
class MetricManager(val storeCreatorCreator: MetricStoreCreatorCreator) extends LazyLogging {
  private var metrics = Map.empty[String, Metric]

  def getOrCreate(conf: Conf): Metric = {
    val storeConf = MetricStoreCreator.newConf(conf.name)
    getOrCreate(conf, storeConf)
  }

  def getOrCreate(conf: Conf, storeConf: MetricStoreCreator.Conf): Metric = this.synchronized {
    val storeCreator = storeCreatorCreator.create(storeConf)
    val metric: Metric = Metric.create(conf, storeCreator)
    metrics += (metric.name -> metric)
    logger.info("registered metric={}", metric.name)
    metric
  }
  def listNames: Seq[String] = metrics.keys.toSeq
  def getMetric(name: String): Option[Metric] = metrics.get(name)

  // todo get from persist or register new
  // todo runtime registration and creation via API
}

object MetricManager {
  def create(storeCreatorCreator: MetricStoreCreatorCreator): MetricManager = new MetricManager(storeCreatorCreator)
}