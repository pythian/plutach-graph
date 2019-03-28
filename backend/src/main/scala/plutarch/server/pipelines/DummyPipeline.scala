package plutarch.server.pipelines

import java.util.concurrent.atomic.AtomicInteger

import scala.language.postfixOps
import akka.actor.{ ActorSystem, Cancellable }
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.ws.WebSocketFlowCoordinator
import plutarch.shared.data.Aggregations
import plutarch.shared.data.metrics.Conf
import plutarch.server.data.experemental.Tools.timing

import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

class DummyPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {

  import system.dispatcher
  import Aggregations._

  val conf: Conf = Conf(
    name = "Dummy",
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    //scales = Seq(1), // 5, 10), //, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    //aggregations = Seq(Sum, Max, Count, PercentileCont(0.1), PercentileCont(0.5), PercentileCont(0.9)),
    aggregations = Seq(Sum, Min, Max, Count, Avg),
    withTotal = true)

  val metric: Metric = metricManager.getOrCreate(conf)

  def publish(t: Long, data: Seq[(String, Double)], silent: Boolean = false): Unit = {
    metric.add(t, data)
    if (!silent) webSocketFlowCoordinator.inform(metric.name)
  }

  private def genData(t: Long) = {
    val arg = t.toDouble / 10000
    Seq(("Sin", 1 + Math.sin(arg)), ("Cos", 1 + Math.cos(arg)))
  }

  def init(cnt: Int = 30000): Unit = timing("init data") {
    val curr0 = System.currentTimeMillis()
    var i = 0
    while (i < cnt) {
      val t = curr0 - (cnt - i) * conf.step
      val data = genData(t)
      publish(t, data, silent = true)
      i += 1
    }
  }

  def start(): Unit = {
    val errCount = new AtomicInteger(0)
    val start: Long = System.currentTimeMillis()
    lazy val schedule: Cancellable = system.scheduler.schedule(0 second, conf.step / 5 millis) {
      val curr = System.currentTimeMillis()
      val data = genData(curr)
      Try(publish(curr, data)) match {
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

  init()
  start()

}
