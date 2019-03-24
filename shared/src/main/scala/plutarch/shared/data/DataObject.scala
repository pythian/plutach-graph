package plutarch.shared.data

case class DataObject(id: Int, name: String, color: String) {
  override def toString: String = s"""{"id": $id, "name": $name, "color": $color"""
}
