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

import akka.actor.{ ActorSystem, Props }
import akka.http.scaladsl.model.ws.Message
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{ Flow, Sink, Source }
import plutarch.server.data.metricManager.MetricManager
import plutarch.shared.Protocol

import scala.concurrent.duration._
import scala.language.postfixOps

trait WebSocketFlowCoordinator {
  def createFlow(req: Option[String]): Flow[String, Message, Any]
  def broadcast(message: Protocol.Broadcastable): Unit
  def inform(metricName: String): Unit
}

object WebSocketFlowCoordinator {
  def create(metricManager: MetricManager)(implicit system: ActorSystem): WebSocketFlowCoordinator = {
    import system.dispatcher

    val sessionCoordinatorRef = system.actorOf(Props(new SessionCoordinator))

    system.scheduler.schedule(0 second, 5 seconds) {
      sessionCoordinatorRef ! Events.BroadcastMessage(Protocol.WSKeepAlive(System.currentTimeMillis()))
    }

    new WebSocketFlowCoordinator {
      def createFlow(req: Option[String]): Flow[String, Message, Any] = {
        val sessionActor = system.actorOf(Props(new Session(req, sessionCoordinatorRef, metricManager)))
        val in =
          Flow[String]
            .map(Events.ReceivedMessage)
            .to(Sink.actorRef[Events.SessionEvent](sessionActor, Events.ConnectionLeft()))
        val out =
          Source.actorRef[Message](100, OverflowStrategy.fail)
            .mapMaterializedValue(outputActor ⇒ sessionActor ! Events.SetOutputActor(outputActor))
        Flow.fromSinkAndSource(in, out)
      }
      def broadcast(message: Protocol.Broadcastable): Unit = sessionCoordinatorRef ! Events.BroadcastMessage(message)
      def inform(metricName: String): Unit = sessionCoordinatorRef ! Events.Inform(metricName)
    }
  }
}
