package plutarch.server.data.accumulators

import plutarch.shared.data.Aggregations.Aggregation

object Accumulators {

  val defaultKnown: Seq[AccumulatorCompanion] = Seq(
    AdditiveAccumulator,
    FirstLastAccumulator,
    PercentileAccumulator)

  private var known: Seq[AccumulatorCompanion] = defaultKnown

  def register(accc: AccumulatorCompanion): Unit = known = accc +: known

  def getMap(aggregations: Seq[Aggregation]): Map[Aggregation, AccumulatorCompanion] = {

    // for each agg getDe list of accs
    val aggToAccComps: Map[Aggregation, Seq[AccumulatorCompanion]] =
      aggregations.map(agg ⇒ agg -> known.filter(_.get.isDefinedAt(agg))).toMap

    // check there is no agg without accs
    aggToAccComps.find(_._2.isEmpty) match {
      case None ⇒
      case Some((agg, _)) ⇒
        throw new Exception(s"Accumulator for agg=$agg has not found")
    }

    // getDe list of accs with no option
    val mustBe: Set[AccumulatorCompanion] =
      aggToAccComps
        .filter(_._2.lengthCompare(1) == 0)
        .flatMap(_._2).toSet

    // getDe agg to acc with options and not covered by mustBe, take any
    val choice =
      aggToAccComps
        .filter(_._2.lengthCompare(1) > 0)
        .filterNot(_._2.exists(mustBe.contains))
        .map(_._2.head)
        .toSet

    val total = mustBe ++ choice

    val aggToAccComp =
      aggToAccComps
        .map(x ⇒ x._1 -> x._2.find(total.contains).get)

    aggToAccComp
  }
}