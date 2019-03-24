package plutarch.client.graph

import plutarch.client.data.{ Data, DataControl, DummyData }
import plutarch.client.graph.Geometry.V
import plutarch.shared.data.{ Aggregations, Charts }

object DummyGraphControl {

  def xLimits(state: Geometry.ContextState): Geometry.V = state.coordinates match {
    case Geometry.CoordinatesUniverseRealTime ⇒ Geometry.V(state.adjCurrent - 3000000000000L, (state.current / state.step) * state.step)
    case Geometry.CoordinatesUniverse         ⇒ Geometry.V(state.adjCurrent - 3000000000000L, state.current + 3000000000000L)
  }

  trait DefaultGraphControlConf extends GraphControlConf {
    Control ⇒

    val drawThrottle = 20
    val mouseThrottle = 10
    val clickDelay = 300
    val minChangePixels = 10
    val minSize = 10000
    def autoResize = true

    val contextInitConf: Geometry.ContextInitConf = new Geometry.ContextInitConf {
      val p0: V = V(50, 100)
      val p1: V = V(150, 70)
      val current: Double = System.currentTimeMillis()
      val gmin: V = V(current - 600000, 0)
      val gmax: V = V(current + 600000, 100)
    }

    val contextLimitsConf: Geometry.ContextLimitsConf = new Geometry.ContextLimitsConf {
      val minSize: Double = Control.minSize
      val minChangePixels: Int = Control.minChangePixels
      def xLimits(state: Geometry.ContextState): V = DummyGraphControl.xLimits(state)
    }

    val selectingConf: SelectingConf = new SelectingConf {
      val minSize: Double = Control.minSize
      val minChangePixels: Int = Control.minChangePixels
      def maxSelectSize: Double = 30000000000L
    }

  }

  def create(initData: Data): GraphControl = {

    val conf: DefaultGraphControlConf = new DefaultGraphControlConf {
      val data: Data = initData
    }

    val graphControl = new GraphControl(conf) {

      def draw(): Unit = {
        import Geometry._
        lineArrow(OP(0, 0), OP(0, graphGeometry.getState.gmax.y))
        //OP(0, 0).angleText(-Math.PI / 2, "RealTimeNow", Font(px = 40, align = "left", baseLine = "bottom"))
      }

    }

    //graphControl.setMetric(DummyData.getMetricDummy("Dummy"))
    //graphControl.setAggregation(Aggregations.Sum)
    //graphControl.setChart(Charts.LayeredAreaChart)
    graphControl.setChart(Charts.StackedAreaChart)

    graphControl
  }

}
