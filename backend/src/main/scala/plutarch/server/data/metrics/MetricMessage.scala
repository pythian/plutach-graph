package plutarch.server.data.metrics

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol

case class MetricMessage(metricName: String, t: Long, values: Seq[(String, Double)])
object MetricMessageJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val metricMessageFormat = jsonFormat3(MetricMessage)
}