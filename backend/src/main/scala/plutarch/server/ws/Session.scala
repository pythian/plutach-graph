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

package plutarch.server.ws

import java.nio.ByteBuffer

import akka.actor.{ Actor, ActorLogging, ActorRef, Stash, Status }
import akka.http.scaladsl.model.ws.{ BinaryMessage, TextMessage }
import akka.util.ByteString
import plutarch.server.data.metricManager.MetricManager
import plutarch.shared.Protocol
import plutarch.shared.Protocol.WSMessage
import upickle.default.{ read, write }
import plutarch.shared.data.Aggregations.Aggregation
import scala.util.Try

class Session(req: Option[String], sessionCoordinatorRef: ActorRef, metricManager: MetricManager) extends Actor with Stash with ActorLogging {
  // there is an issue with NPE after actor shutdown if some futures calculating (be advised!)
  import context.dispatcher
  private var outputActor: Option[ActorRef] = None
  private var id: Option[String] = None
  private var isReady = false
  private var isInitClient = false
  private val request = {
    val res = req match {
      case Some(str) ⇒
        Try(read[Protocol.WSOpenRequest](str))
          .recover {
            case ex ⇒
              val msg = s"Unable to parse open request=${req.get}, exception=$ex"
              Protocol.WSInvalidRequest(msg)
          }.get
      case None ⇒
        Protocol.WSEmptyRequest()
    }
    log.info("Open session with request {}", res)
    res
  }
  val informThreshold: Long = -1000
  private var lastInform: Long = Long.MinValue
  private var rtsubscriptions = Map.empty[String, Map[(Aggregation, Int), Set[String]]]

  private def stopSession(): Unit = {
    sessionCoordinatorRef ! Events.SessionCompleted(self)
    outputActor.foreach(_ ! Status.Success(Unit))
    context.stop(self)
  }

  private def sendTextMessage(msg: String): Unit = outputActor match {
    case Some(ref) ⇒
      ref ! TextMessage.Strict(msg.toString)
    case None ⇒
      val sessionId = id.getOrElse("Not registered")
      log.error("Unable to send text message to output actor from sessionId={} as it has not set yet", sessionId)
      stopSession()
  }

  private def sendBinaryMessage(buffer: ByteBuffer): Unit = outputActor match {
    case Some(ref) ⇒
      val bs = ByteString.fromByteBuffer(buffer)
      ref ! BinaryMessage.Strict(bs)
    case None ⇒
      val sessionId = id.getOrElse("Not registered")
      log.error("Unable to send text message to output actor from sessionId={} as it has not set yet", sessionId)
      stopSession()
  }

  private def initClient(): Unit = if (!isInitClient) {
    val metrics = metricManager.listNames
    val msg = Protocol.WSAccepted(id.get, metrics)
    sendTextMessage(write(msg))
    unstashAll()
    isInitClient = true
  }

  def receive: Receive = {
    case Events.SetOutputActor(ref) ⇒
      outputActor = Some(ref)
      request match {
        case Protocol.WSInvalidRequest(str) ⇒
          val msg = write(Protocol.WSRejected(str))
          sendTextMessage(msg)
          stopSession()
        case _ ⇒
          sessionCoordinatorRef ! Events.Connection(self)
      }
    case Events.ReceivedMessage(msg) ⇒ id match {
      case Some(sessionId) ⇒
        //log.debug("Session sessionId={} received msg={}", sessionId, msg)
        read[WSMessage](msg) match {
          case Protocol.WSReady() ⇒
            isReady = true
            if (id.nonEmpty) initClient()
          case Protocol.WSSubscribe(metrics) ⇒
            log.debug("Session is interested in the following metrics={}", metrics.mkString("[", ", ", "]"))
            val confs = for {
              name ← metrics
              metric ← metricManager.getMetric(name)
            } yield {
              name -> metric.meta
            }
            val msg = Protocol.WSMetricsConfs(confs.toMap)
            sendTextMessage(write(msg))
            sessionCoordinatorRef ! Events.Subscribe(self, metrics)
          case Protocol.WSSubscribeRealTime(metric, aggregation, scale, clientId, active) ⇒
            //log.debug(s"Subscribed metric=$metric, aggregation=$aggregation, scale=$scale, clientId=$clientId, active=$active")
            var aggs = rtsubscriptions.getOrElse(metric, Map.empty)
            val key = (aggregation, scale)
            val set = aggs.getOrElse(key, Set.empty)
            if (active) {
              aggs += key -> (set + clientId)
            } else {
              val newSet = set - clientId
              if (newSet.isEmpty) {
                aggs -= key
              } else {
                aggs += key -> newSet
              }
            }
            if (aggs.isEmpty) {
              rtsubscriptions -= metric
            } else {
              rtsubscriptions += metric -> aggs
            }
          case Protocol.WSHistRequest(requestId, metricName, aggregation, scale, intervals) ⇒
            //log.debug(s"incoming history request $msg")
            metricManager.getMetric(metricName) match {
              case Some(metric) ⇒
                val longIntervals = intervals.map(i ⇒ (i.left.toLong, i.right.toLong))
                if (longIntervals.size == 1) {
                  val (x, y) = longIntervals.head
                  for (buffer ← metric.get(aggregation, requestId, scale, x, y)) {
                    sendBinaryMessage(buffer)
                  }
                } else {
                  for (buffer ← metric.get(aggregation, requestId, scale, longIntervals)) {
                    sendBinaryMessage(buffer)
                  }
                }
              case None ⇒
                log.info(s"Requested non-existing metricName=$metricName")
            }
          case Protocol.WSLoggingToServer(logmsg) ⇒
            log.info(logmsg)
          case wsMessage ⇒
            log.info("Session received message it has not specific handler for={}", wsMessage)
        }
      case None ⇒
        log.info("Session received message before assigned sessionId, msg={}", msg)
        stash()
    }
    case Events.SessionBroadcastMessage(msgSerialized) ⇒
      val sessionId = id.get
      //log.debug("Session sessionId={} received broadcasted msg={}", sessionId, msgSerialized)
      sendTextMessage(msgSerialized)
    case Events.Inform(metricName) ⇒
      // taken by time! and not throttling
      val currentTime = System.currentTimeMillis()
      if (lastInform < currentTime - informThreshold) {
        lastInform = currentTime
        for (metric ← metricManager.getMetric(metricName)) {
          val currents = for {
            subs: Map[(Aggregation, Int), Set[String]] ← rtsubscriptions.get(metricName).toIterable
            (agg, scale) ← subs.keys
          } yield {
            val ((key, version, data), objects) = metric.getCurrent(agg)(scale)
            val currentData: Map[Int, Double] = data.view.map(x ⇒ (x._1, agg.getDouble(x._2))).toMap
            Protocol.CurrentData(agg, scale, key, version, currentData, objects)
          }
          val msg = Protocol.WSCurrents(metricName, metric.getCurrentKey, currents.toSeq)
          sendTextMessage(write(msg))
        }
      }
    case Events.Accepted(sessionId) ⇒
      id = Some(sessionId)
      log.debug("Session accepted with sessionId={}", sessionId)
      if (isReady) initClient()
    case Events.ConnectionLeft() ⇒
      val sessionId = id.get
      log.info("Connection left for sessionId={}", sessionId)
      stopSession()
    case Status.Failure(ex) ⇒
      val sessionId = id.getOrElse("Not registered")
      log.info("Connection failed for sessionId={}", sessionId, ex)
      stopSession()
  }
}

