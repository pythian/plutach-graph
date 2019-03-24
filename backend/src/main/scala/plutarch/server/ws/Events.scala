package plutarch.server.ws

import akka.actor.ActorRef
import plutarch.shared.Protocol

object Events {
  sealed trait CoordinatorEvent
  case class BroadcastMessage(message: Protocol.Broadcastable) extends CoordinatorEvent
  case class Connection(sessionActorRef: ActorRef) extends CoordinatorEvent
  case class SessionCompleted(sessionActorRef: ActorRef) extends CoordinatorEvent
  case class Subscribe(sessionActorRef: ActorRef, metrics: Seq[String]) extends CoordinatorEvent

  sealed trait SessionEvent
  case class SessionBroadcastMessage(messageSerialized: String) extends SessionEvent
  case class SetOutputActor(ref: ActorRef) extends SessionEvent
  case class Accepted(sessionId: String) extends SessionEvent
  case class ReceivedMessage(message: String) extends SessionEvent
  case class ConnectionLeft() extends SessionEvent

  case class Inform(metricName: String) extends CoordinatorEvent with SessionEvent
}