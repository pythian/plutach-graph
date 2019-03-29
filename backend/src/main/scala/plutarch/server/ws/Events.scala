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