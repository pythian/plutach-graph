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