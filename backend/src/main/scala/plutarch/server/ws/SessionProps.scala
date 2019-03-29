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