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