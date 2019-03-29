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

package plutarch.client.data

import plutarch.shared.colors.Colors
import plutarch.shared.data.DataObject

object DataObjectOps {
  def apply(underlined: DataObject): DataObjectOps = new DataObjectOps(underlined)
}

class DataObjectOpsState(underlined: DataObject) {
  var selected = false
  var order: Int = underlined.id
}

class DataObjectOps(underlined: DataObject) {
  private val state = new DataObjectOpsState(underlined)
  private lazy val colors: Array[String] = Colors.intensities(underlined.color, 0.5, 0.4)
  def getName: String = underlined.name
  def getColor1: String = if (state.selected) Colors.selectColor else underlined.color
  def getColor2: String = if (state.selected) Colors.selectColor else colors(0)
  def getColor3: String = if (state.selected) Colors.selectColor else colors(1)
  def setSelected(b: Boolean): Unit = state.selected = b
  def getOrder: Int = state.order
  def setOrder(order: Int): Unit = state.order = order
}

