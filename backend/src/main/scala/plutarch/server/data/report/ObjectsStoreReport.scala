package plutarch.server.data.report

sealed trait ObjectsStoreReport

case class MemoryObjectsStoreReport(colorsSize: Int, idsSize: Int, timesSize: Int) extends ObjectsStoreReport