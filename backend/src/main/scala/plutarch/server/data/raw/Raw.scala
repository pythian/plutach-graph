package plutarch.server.data.raw

import scala.concurrent.Future

// todo: fast raw to scales converter
trait Raw {
  def put(t: Long, values: Seq[(String, Double)]): Future[Unit]
  def first: Long
  def current: Long
  def iterator: Iterator[(Long, Seq[(String, Double)])]
}

object Raw {
  def create(name: String): Raw = new Impl(name)

  class DummyRawImpl extends Raw {
    var first: Long = Long.MaxValue
    val current: Long = Long.MinValue
    def put(t: Long, values: Seq[(String, Double)]): Future[Unit] = {
      if (first == Long.MaxValue) first = t
      Future.successful()
    }
    def iterator: Iterator[(Long, Seq[(String, Double)])] = Iterator.empty
  }

  class Impl(name: String) extends Raw {
    var first: Long = Long.MaxValue
    var current: Long = Long.MinValue
    private var store: List[(Long, Seq[(String, Double)])] = Nil
    def put(t: Long, values: Seq[(String, Double)]): Future[Unit] = {
      if (first == Long.MaxValue) first = t
      store = (t, values) :: store
      current = current max t
      Future.successful()
    }
    def iterator: Iterator[(Long, Seq[(String, Double)])] = store.iterator
  }
}