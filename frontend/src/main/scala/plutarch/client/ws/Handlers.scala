package plutarch.client.ws

import plutarch.client.Client
import plutarch.client.data.CacheData
import plutarch.client.graph.GraphControl
import plutarch.client.ws.WS.Message
import plutarch.shared.Protocol
import plutarch.shared.data.Picklers
import slogging.LazyLogging
import upickle.default.{ read, write }

import scala.util.Try

object Handlers extends LazyLogging {
  def wsHandler(ws: WS, cacheData: CacheData, graphControl: GraphControl): Message ⇒ Unit = {
    case WS.TextMessage(msg) ⇒
      //logger.info("WebSocket got message {}", msg)
      val decoded = read[Protocol.WSMessage](msg)
      decoded match {
        case accepted: Protocol.WSAccepted ⇒
          Client.clientId = accepted.id
          ws.send(write(Protocol.WSSubscribe(accepted.metrics))) // subscribe everything!
        case alive: Protocol.WSKeepAlive ⇒ // ok
        case mconfs: Protocol.WSMetricsConfs ⇒
          for (meta ← mconfs.confs.values if !cacheData.isRegistered(meta)) {
            cacheData.registerMetric(meta)
          }
          if (!graphControl.isMetricSet) {
            graphControl.setMetricAndAggregation(mconfs.confs.values.head.conf)
          }
        case data: Protocol.WSData     ⇒ // ok
        case curr: Protocol.WSCurrents ⇒ cacheData.receive(curr)
        case other                     ⇒ logger.info("get other TextMessage {}", other)
      }
    case WS.BinaryMessage(buffer) ⇒
      val combined: Picklers.CombinedData[_] = Picklers.extract(buffer)
      cacheData.receive(combined)
  }
}
