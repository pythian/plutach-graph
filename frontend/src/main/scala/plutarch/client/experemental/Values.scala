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

abstract class Values[+V >: Null] {
  def getOrElse[T >: V](k: Double, default: T): T
  def from[T >: V](k: Double, step: Double, default: T): () ⇒ T
}

trait MValues[V >: Null] {
  def set(k: Double, v: V): Unit
  def delete(k: Double): Unit
}

object Values {

  object Empty extends Values[Null] {
    override def getOrElse[T >: Null](k: Double, default: T): T = default
    override def from[T >: Null](k: Double, step: Double, default: T): () ⇒ T = () ⇒ default
  }

}