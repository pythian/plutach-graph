package plutarch.server.pipelines

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorSystem, Cancellable}
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations
import plutarch.shared.data.metrics.Conf
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ExternalPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {

  import Aggregations._

  val conf: Conf = Conf(
    name = "External",
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    aggregations = Seq(Sum, Min, Max, Count, Avg,
      //PercentileCont(0.01),
      //PercentileCont(0.05),
      //PercentileCont(0.10),
      //PercentileCont(0.25),
      //PercentileCont(0.50),
      //PercentileCont(0.75),
      //PercentileCont(0.90),
      //PercentileCont(0.95),
      //PercentileCont(0.99),
      //Stdev,
      //First,
      //Last
    ),
    withTotal = false)

  val metric: Metric = metricManager.getOrCreate(conf)

  def publish(t: Long, data: Seq[(String, Double)], silent: Boolean = false): Unit = {
    metric.add(t, data)
    if (!silent) webSocketFlowCoordinator.inform(metric.name)
  }

  def start(): Unit = {
    val errCount = new AtomicInteger(0)
    val start: Long = System.currentTimeMillis()
    lazy val schedule: Cancellable = system.scheduler.schedule(0 second, conf.step / 5 millis) {
      val curr = System.currentTimeMillis()
      val data = metric.pop()
      val totalData = data.flatMap(_._2.groupBy(_._1).mapValues(v => v.map(_._2).sum).toList)
      Try(publish(curr, totalData)) match {
        case Success(_) ⇒
        case Failure(ex) ⇒
          logger.error("Getting error while publishing", ex)
          val errors = errCount.addAndGet(1)
          if (errors > 5) {
            logger.error("Too many errors, canceling pipeline")
            schedule.cancel()
          }
      }
    }
    schedule
  }

}
