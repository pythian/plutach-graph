package plutarch.server.pipelines.spark

import java.nio.charset.Charset
import java.util.concurrent.{ TimeUnit, TimeoutException }
import akka.actor.ActorSystem
import com.google.cloud.pubsub.v1.{ AckReplyConsumer, MessageReceiver, Subscriber }
import com.google.pubsub.v1.{ ProjectSubscriptionName, PubsubMessage }
import org.apache.commons.lang.exception.ExceptionUtils
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.data.metrics.Metric
import plutarch.server.pipelines.spark.PubSubConnection.ParseFn
import plutarch.server.ws.WebSocketFlowCoordinator
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import PubSubConnectionImpl._

object PubSubConnectionImpl {

  case class Config(publishTimeout: Int)

  class Receiver(metricName: String, config: Config)(implicit executionContext: ExecutionContext, metricManager: MetricManager, webSocketFlowCoordinator: WebSocketFlowCoordinator)
    extends DebugBuffer {

    val metric: Metric = metricManager.getMetric(metricName).getOrElse(
      throw new RuntimeException(s"Unknown metric $metricName"))

    var lastKey: Long = metric.getCurrentKey
    val buffer = new java.util.TreeMap[Long, Seq[(String, Double)]]
    var lastPublished: Long = System.currentTimeMillis()
    var keepInBuffer: Boolean = true

    def receive(rawT: Long, data: Seq[(String, Double)]): Unit = {
      val t = (rawT / metric.conf.step) * metric.conf.step

      if (keepInBuffer) {
        // replace, not aggregate
        buffer.put(t, data)
      } else if (t == lastKey + metric.conf.step) {
        metric.add(t, data)
        lastKey = t
        if (!buffer.isEmpty && buffer.firstKey() == lastKey + metric.conf.step) {
          flushBuffer(continuously = true, failfast = true)
        }
        webSocketFlowCoordinator.inform(metric.name)
        lastPublished = System.currentTimeMillis()
      } else if (t > lastKey + metric.conf.step) {
        buffer.put(t, data)
        if (lastPublished < System.currentTimeMillis() - config.publishTimeout) {
          flushBuffer(continuously = false, failfast = true)
        }
      } else {
        log(s"Received late data for key=$rawT, t=$t, lastKey=$lastKey")
      }
    }

    private def traverseBuffer(continuously: Boolean, failfast: Boolean): List[Long] = {
      var keysToDelete = List.empty[Long]
      for (entry ← buffer.entrySet().asScala) {
        if (entry.getKey == lastKey + metric.conf.step || !continuously) {
          if (failfast) {
            metric.add(entry.getKey, entry.getValue)
          } else {
            try {
              metric.add(entry.getKey, entry.getValue)
            } catch {
              case ex: Exception ⇒
                log(s"Failed to load entry.getKey=${entry.getKey}, entry.getValue=${entry.getValue} during flushBuffer, ex=$ex")
                log(ExceptionUtils.getStackTrace(ex))
            }
          }
          if (continuously) {
            keysToDelete = entry.getKey :: keysToDelete
          }
          lastKey = entry.getKey
        } else if (continuously) {
          return keysToDelete
        }
      }
      keysToDelete
    }

    def flushBuffer(continuously: Boolean, failfast: Boolean): Unit = {
      val keysToDelete = traverseBuffer(continuously, failfast)
      if (continuously) {
        keysToDelete.foreach(buffer.remove)
      } else {
        buffer.clear()
      }
      webSocketFlowCoordinator.inform(metric.name)
      lastPublished = System.currentTimeMillis()
      this.keepInBuffer = false
    }

  }

}

class PubSubConnectionImpl(config: Config, subscriptionName: ProjectSubscriptionName, metrics: Seq[String], parseFn: ParseFn)(implicit system: ActorSystem, metricManager: MetricManager, webSocketFlowCoordinator: WebSocketFlowCoordinator)
  extends PubSubConnection with DebugBuffer {
  conn ⇒

  import system.dispatcher

  val receivers: Map[String, Receiver] = metrics.map(name ⇒ name -> new Receiver(name, config)).toMap

  val messageReceiver: MessageReceiver = new MessageReceiver {

    override def receiveMessage(message: PubsubMessage, consumer: AckReplyConsumer): Unit = conn.synchronized {
      try {
        val json = message.getData.toString(Charset.forName("UTF-8"))
        val (metricName, rawT, data) = try {
          parseFn(json) match {
            case Some(res) ⇒
              res
            case None ⇒
              return
          }
        } catch {
          case ex: Exception ⇒
            log(s"Error during parsing message=$json, ex=$ex")
            ex.printStackTrace()
            return
        } finally {
          consumer.ack()
        }

        receivers
          .get(metricName)
          .foreach(_.receive(rawT, data))

      } catch {
        case ex: Exception ⇒
          log(s"Error on receiveMessage ex=${ex.getMessage} \n${ExceptionUtils.getFullStackTrace(ex)}")
      }
    }
  }

  def init(): Subscriber = {
    val subscriber: Subscriber = Subscriber.newBuilder(subscriptionName, messageReceiver).build()
    subscriber.startAsync()
    try {
      subscriber.awaitTerminated(5000, TimeUnit.MILLISECONDS)
    } catch {
      case _: TimeoutException ⇒
    }
    subscriber
  }

  val subscriber: Subscriber = init()

  def flushBuffers(failfast: Boolean = true, atMost: Duration = 10 minutes): Unit = conn.synchronized {
    if (receivers.size > 1) {
      // async for > 1
      Await.result(Future.sequence(receivers.values.map(r ⇒ Future(r.flushBuffer(continuously = false, failfast)))), atMost)
    } else {
      // sync for 1
      receivers.values.foreach(r ⇒ r.flushBuffer(continuously = false, failfast))
    }
  }

  def setKeepBuffers(): Unit = {
    receivers.values.foreach(r ⇒ r.keepInBuffer = true)
  }
}
