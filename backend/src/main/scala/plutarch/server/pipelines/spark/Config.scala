package plutarch.server.pipelines.spark

import scala.concurrent.duration._

trait Conf {
  def batchSize: Int
  def timeout: Duration
}

object Config {
  private val BATCH_SIZE = 100
  private val TIMEOUT = 10 seconds

  val default: Conf = new Conf {
    override val batchSize: Int = BATCH_SIZE
    override val timeout: Duration = TIMEOUT
  }

}
