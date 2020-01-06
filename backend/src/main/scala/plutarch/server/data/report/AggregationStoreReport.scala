package plutarch.server.data.report

case class PageReport(offsetsCapacity: Int, byteBufferCapacity: Int) {
  override def toString: String = {
    s"""{"offsetsCapacity": $offsetsCapacity, "byteBufferCapacity": $byteBufferCapacity}"""
  }
}

sealed trait AggregationStoreReport
case class ByteBufferAggregationStoreReport() extends AggregationStoreReport
case class MultiByteBufferAggregationStoreReport(pages: Map[Long, PageReport]) extends AggregationStoreReport
case class FileAggregationStoreReport() extends AggregationStoreReport