package plutarch.client

import org.scalajs.dom
import dom._
import org.scalajs.dom.html.Div
import plutarch.client.data.{ CacheData, DataSource, DummyData }
import slogging._
import plutarch.client.ws.{ Handlers, WS }
import plutarch.shared.Protocol
import scalatags.JsDom.all._

import scala.scalajs.js.timers._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global

object Client extends LazyLogging {

  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.DEBUG

  var clientId = "unset"

  def main(args: Array[String]): Unit = {
    // initialize websocket with message handlers
    val ws: WS = WS.create()
    val container = div().render
    val pl = div(h1("PLUTARCH")).render
    window.onload = _ ⇒ {
      document.body.appendChild(pl)
      document.body.appendChild(container)
    }
    ws.ready.foreach { _ ⇒
      initialization(ws, container, pl)
      //initialization0(ws, container)
    }
  }

  def initialization0(ws: WS, container: Div): Unit = {

    val data = DummyData.createDummy(0.000001, 100, 10)
    val dummyGraphControl = graph.DummyGraphControl.create(data) // data control is not used for dummy
    container.appendChild(dummyGraphControl.root)

    setTimeout(0) {
      dummyGraphControl.setMetric(data.get("Dummy").getMetricConf)
      dummyGraphControl.setAggregation(data.get("Dummy").getMetricConf.aggregations.head)
      dummyGraphControl.setSize()
    }

  }

  def initialization(ws: WS, container: Div, pl: Div): Unit = {

    val dataSource = DataSource.create(ws)
    val cacheData = CacheData.create(dataSource)
    val dummyGraphControl = graph.DummyGraphControl.create(cacheData)
    container.appendChild(dummyGraphControl.root)

    pl.onclick = (e: MouseEvent) ⇒ {
      val m = cacheData.state.metrics.toIndexedSeq.zipWithIndex
      val sz = m.length
      dummyGraphControl.getMetric.foreach { name ⇒
        m.find(_._1._1 == name).foreach { e ⇒
          val idx = (e._2 + 1) % sz
          dummyGraphControl.setMetricAndAggregation(m(idx)._1._2.getMetricConf)
        }
      }
    }

    cacheData.setOnReceive(dummyGraphControl.update)
    cacheData.setOnCurrentsReceive { req ⇒
      if (dummyGraphControl.state.metric.map(_.name).contains(req.metric)) {
        dummyGraphControl.setCurrent(req.key)
      }
    }

    ws.initHandler(Handlers.wsHandler(ws, cacheData, dummyGraphControl))
    ws.sendReady()
    ws.reconnected { () ⇒
      cacheData.reset()
      dummyGraphControl.reset()
    }

    // finalize initialization
    setTimeout(0) {
      dummyGraphControl.setSize()
    }

    val kstep = Protocol.CLIENT_CURRENT_INTERVAL
    setInterval(kstep) {
      dummyGraphControl.setCurrent(kstep * (System.currentTimeMillis() / kstep))
    }

  }

}