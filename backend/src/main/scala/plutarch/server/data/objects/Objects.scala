package plutarch.server.data.objects

import java.nio.ByteBuffer
import plutarch.shared.data.DataObject
import scala.concurrent.{ ExecutionContext, Future }

// todo: combine with permanent store!
trait Objects {
  def check(t: Long, name: String): DataObject = check(t, name, None)
  def check(t: Long, name: String, initColor: Option[String]): DataObject
  def getObject(name: String): DataObject
  def get(left: Long, right: Long)(implicit executor: ExecutionContext): Future[ByteBuffer]
  def get(intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[ByteBuffer]
  def getDe(left: Long, right: Long): Seq[DataObject]
  def getDe(intervals: Seq[(Long, Long)]): Seq[DataObject]
}

object Objects {

  val latency = 60000

  def create(name: String): Objects = {
    new ImmutableObjectStore(name)
  }

}