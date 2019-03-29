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

import plutarch.shared.data.DataObject

/*
* Immutable threadsafe implementation: 1 writer/many readers
* stat is published via volatile variable
* */

class ImmutableObjectStore(name: String) extends MemoryObjectStore(0, MemoryObjectStore.emptyState, Objects.latency) {

  def check(t: Long, name: String, initColor: Option[String]): DataObject = {
    checkState(t, name, initColor).state.obj
  }

  // setup total
  check(Long.MinValue, "total", Some("#43CD80"))
  check(Long.MaxValue, "total")
}