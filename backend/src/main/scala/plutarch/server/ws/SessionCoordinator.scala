package plutarch.server.ws

import java.util.UUID
import akka.actor.{ Actor, ActorRef, Terminated }
import com.typesafe.scalalogging.LazyLogging
import upickle.default.{ read, write }

/**
 * Session life cycle:
 * 1. created session actor
 * 2. session actor received (Flow: SetOutputActor -> session)
 * 3. session starting to listen incoming ws messages (Flow: ReceivedMessage -> session)
 * 4. session registered at sessions coordinator with Connection (session: Connection -> coordinator)
 * 5. session received Accepted from coordinator (coordinator: Accepted -> session)
 * 6. coordinator starting to broadcast messages to this session too (outer: BroadcastMessage -> coordinator -> session)
 * 7. session may receive Status.Failure, then it's completed (Flow: Status.Failure -> session)
 * 8. connection can be closed so session will receive ConnectionLeft, then it's completed (Flow: ConnectionLeft -> session)
 * 9. once session completed, it sending SessionCompleted to coordinator (session: SessionCompleted -> coordinator)
 *    it sends message Status.Success(Unit) to output actor to close the connection and stop itself actor
 * 10. coordinator also watching session actors, so it receives Terminated once session actor stopped (akka: Terminated -> coordinator)
 *     normally it should happen after SessionCompleted received, otherwise it's logged
 */

class SessionCoordinator extends Actor with LazyLogging {
  private var sessions = Map.empty[ActorRef, SessionProps]
  private def generateId(): String = UUID.randomUUID().toString
  def receive: Actor.Receive = {
    case Events.BroadcastMessage(msg) ⇒
      val msgSerialized = Events.SessionBroadcastMessage(write(msg))
      //logger.debug("coordinator got broadcasted message, msgSerialized={}", msgSerialized)
      sessions.filter(_._2.isInterested(msg)).keys.foreach(ref ⇒ ref ! msgSerialized)
    case inform @ Events.Inform(metricName) ⇒
      sessions.filter(_._2.isInterested(metricName)).keys.foreach(ref ⇒ ref ! inform)
    case Events.Connection(sessionActorRef) ⇒
      context.watch(sessionActorRef)
      val sessionId = generateId()
      sessions += sessionActorRef -> SessionProps(sessionId)
      sessionActorRef ! Events.Accepted(sessionId)
      logger.info("New connection, sessionId={}", sessionId)
    case Events.Subscribe(sessionActorRef, metrics) ⇒
      val props = sessions(sessionActorRef)
      sessions += (sessionActorRef -> props.addMetrics(metrics))
      logger.info("Session sessionId={} subscrided to metrics", props.id, metrics.mkString("[", ",", "]"))
    case Events.SessionCompleted(sessionActorRef) ⇒
      val sessionId = sessions.get(sessionActorRef).map(_.id).getOrElse("Not registered")
      sessions -= sessionActorRef
      logger.info("Connection left, sessionId={}", sessionId)
    case Terminated(ref) ⇒
      sessions.get(ref).map(_.id) match {
        case Some(sessionId) ⇒
          logger.error("For a some reason session wasn't removed with SessionCompleted but actor is terminated for sessionId={}", sessionId)
          sessions -= ref
        case None ⇒
          logger.info("Terminated session what was already removed from sessions or was never registered")
      }
    case other ⇒
      logger.warn("Get unknown message: {}", other)
  }
}
