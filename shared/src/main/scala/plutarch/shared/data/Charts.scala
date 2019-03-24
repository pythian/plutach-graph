package plutarch.shared.data

object Charts {

  sealed trait Chart {
    def name: String
    def color: String
  }

  case object LineChart extends Chart {
    val name: String = "L"
    def color: String = "rgba(10,250,55,0.7)"
  }
  case object StackedAreaChart extends Chart {
    val name: String = "SA"
    def color: String = "rgba(250,10,55,0.7)"
  }
  case object LayeredAreaChart extends Chart {
    val name: String = "LA"
    def color: String = "rgba(10,80,250,0.7)"
  }
  case object SimpleLineChart extends Chart {
    val name: String = "SL"
    def color: String = "rgba(250,10,250,0.7)"
  }

  val flagToChart = Map(0 -> LineChart, 1 -> LayeredAreaChart, 2 -> StackedAreaChart, 3 -> SimpleLineChart)

}
