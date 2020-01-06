package plutarch.server.data.report

import plutarch.shared.data.Aggregations.Aggregation

case class AggregationsStoreReport(aggregationStores: Map[Aggregation, AggregationStoreReport])