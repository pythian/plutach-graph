package plutarch.shared.data.metrics

import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.MetricDomain._

case class Conf(
    name:         String,
    step:         Long,
    scales:       Seq[Int],
    aggregations: Seq[Aggregation],
    withTotal:    Boolean,
    unitSelector: UnitSelector     = DefaultUnitSelector,
    dataFormat:   DataFormat       = DefaultDataFormat)