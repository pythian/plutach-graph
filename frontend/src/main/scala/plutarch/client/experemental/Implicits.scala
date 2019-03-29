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

package plutarch.client.experemental

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success, Try }
import scala.scalajs.js.timers.setTimeout

object Implicits {

  def init[T](value: T): Future[T] = Future.successful(value)

  implicit class RichFuture[T](f: Future[T]) {
    def afterMap[U](delay: Double)(action: T ⇒ U)(implicit executor: ExecutionContext): Future[U] = {
      val p = Promise[U]()
      f.onComplete {
        case Success(res) ⇒ setTimeout(delay)(p.complete(Try(action(res))))
        case Failure(ex)  ⇒ p.complete(Failure(ex))
      }
      p.future
    }
    def afterSeqMap[U <: T](delay: Double, times: Int)(action: T ⇒ U)(implicit executor: ExecutionContext): Future[U] = {
      (1 until times).foldLeft(f)((f, _) ⇒ f.afterMap(delay)(action)).afterMap(delay)(action)
    }
  }

  implicit class ExtendedIterable[T](underlying: Iterable[T]) {
    def reverse: Seq[T] = {
      underlying.foldLeft(List.empty[T])((acc, elem) ⇒ elem :: acc)
    }

    //    def join(that: Iterable[T]): Iterable[T] = new Iterable[T] {
    //      def iterator: Iterator[T] = underlying.iterator ++ that.iterator
    //    }

    //    def join(that: Iterable[T], lastLeft: T)(implicit cmp: Ordering[T]): Iterable[T] = new Iterable[T] {
    //      def iterator: Iterator[T] =
    //        underlying.iterator.filter(cmp.compare(_, lastLeft) <= 0) ++
    //          that.iterator.filter(cmp.compare(_, lastLeft) > 0)
    //    }

    def optionMaxBy[B](f: T ⇒ B)(implicit cmp: Ordering[B]): Option[T] = {
      if (underlying.isEmpty) None else Some(underlying.maxBy(f))
    }

    def optionMinBy[B](f: T ⇒ B)(implicit cmp: Ordering[B]): Option[T] = {
      if (underlying.isEmpty) None else Some(underlying.minBy(f))
    }

    def optionMax(implicit cmp: Ordering[T]): Option[T] = {
      if (underlying.isEmpty) None else Some(underlying.max)
    }

    def optionMin(implicit cmp: Ordering[T]): Option[T] = {
      if (underlying.isEmpty) None else Some(underlying.min)
    }
  }

}