package plutarch.server.data.report

sealed trait RawStoreReport
case class RawStoreDummyReport() extends RawStoreReport
case class RawStoreImplReport(size: Int) extends RawStoreReport
case class RawStoreFileReport() extends RawStoreReport
