package plutarch.client.data

import plutarch.client.ws.WS
import plutarch.shared.Protocol
import plutarch.shared.Protocol.Interval
import plutarch.shared.data.Aggregations.Aggregation
import upickle.default.write

trait DataSource {
  def request(metricName: String, aggregation: Aggregation, scale: Int, intervals: Seq[Interval]): Protocol.WSHistRequest
  def subscribe(metricName: String, aggregation: Aggregation, scale: Int, clientId: String): Unit
  def unsubscribe(metricName: String, aggregation: Aggregation, scale: Int, clientId: String): Unit
}

object DataSource {
  private var currentRequestId = 0
  def create(ws: WS): DataSource = {
    new DataSource {
      def request(metricName: String, aggregation: Aggregation, scale: Int, intervals: Seq[Interval]): Protocol.WSHistRequest = {
        val requestId = currentRequestId
        currentRequestId += 1
        val req = Protocol.WSHistRequest(requestId, metricName, aggregation, scale, intervals)
        val msg = write(req)
        ws.send(msg)
        req
      }
      def subscribe(metricName: String, aggregation: Aggregation, scale: Int, clientId: String): Unit = {
        val req = Protocol.WSSubscribeRealTime(metricName, aggregation, scale, clientId, active = true)
        val msg = write(req)
        ws.send(msg)
      }
      def unsubscribe(metricName: String, aggregation: Aggregation, scale: Int, clientId: String): Unit = {
        val req = Protocol.WSSubscribeRealTime(metricName, aggregation, scale, clientId, active = false)
        val msg = write(req)
        ws.send(msg)
      }
    }
  }
}