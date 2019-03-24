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

