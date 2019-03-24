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
