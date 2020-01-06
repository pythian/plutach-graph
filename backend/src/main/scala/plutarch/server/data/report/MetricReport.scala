package plutarch.server.data.report

case class MetricReport(rawStore: RawStoreReport, scalesStore: Map[Int, ScaleReport], objectsStore: ObjectsStoreReport) {
  def scalesBuffersSizeMb: Long = {
    val buffers = for {
      s ← scalesStore.values.toList
      a ← s.aggregationsStore.aggregationStores.values
      if a.isInstanceOf[MultiByteBufferAggregationStoreReport]
      p ← a.asInstanceOf[MultiByteBufferAggregationStoreReport].pages.values
    } yield {
      p.byteBufferCapacity + p.offsetsCapacity
    }
    buffers.map(_.toLong).sum / 1024 / 1024
  }

  override def toString: String = {
    ScalaGson.gson.toJson(this)
  }

  def toPrettyString: String = {
    ScalaGson.prettyGson.toJson(this)
  }

}