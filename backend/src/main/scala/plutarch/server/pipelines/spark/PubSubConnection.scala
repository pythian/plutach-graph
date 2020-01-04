package plutarch.server.pipelines.spark

import com.google.cloud.pubsub.v1.Subscriber

object PubSubConnection {
  type ParseFn = String ⇒ Option[(String, Long, Seq[(String, Double)])]
}

trait PubSubConnection {
  def subscriber: Subscriber
}