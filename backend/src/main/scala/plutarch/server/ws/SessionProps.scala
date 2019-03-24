package plutarch.server.ws

import plutarch.shared.Protocol

case class SessionProps(id: String, metrics: Set[String]) {
  def isInterested(message: Protocol.Broadcastable): Boolean = message match {
    case Protocol.WSKeepAlive(_)          ⇒ true
    case Protocol.WSData(metric, _, _, _) ⇒ metrics.contains(metric)
    case Protocol.WSTextMessage(_)        ⇒ false
  }
  def isInterested(metricName: String): Boolean = metrics.contains(metricName)
  def addMetrics(metrics: Seq[String]): SessionProps = SessionProps(id, this.metrics ++ metrics)
}

object SessionProps {
  def apply(id: String): SessionProps = SessionProps(id, Set.empty)
}