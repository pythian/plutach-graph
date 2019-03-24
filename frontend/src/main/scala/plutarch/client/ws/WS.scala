package plutarch.client.ws

import java.nio.ByteBuffer

import org.scalajs.dom
import org.scalajs.dom.{ Blob, Event, MessageEvent, WebSocket }
import plutarch.shared.Protocol
import slogging.LazyLogging
import upickle.default.write

import scala.scalajs.js.timers._
import scala.concurrent.{ Future, Promise }
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
import scala.util.{ Failure, Success, Try }

trait WS {
  def uri: String
  def created: Long
  def connected: Long
  def send(msg: String)
  def ready: Future[Unit]
  def reconnected(callback: () ⇒ Unit): Unit
  def initHandler(handler: WS.Message ⇒ Unit): Unit
  def sendReady(): Unit
  def serverLog(msg: String): Unit
}

object WS extends LazyLogging {
  sealed trait Message
  case class TextMessage(msg: String) extends Message
  case class BinaryMessage(buffer: ByteBuffer) extends Message

  private var last: Option[WS] = None
  def serverLog(msg: String): Unit = last.foreach(_.serverLog(msg))

  private def init(uri: String, onOpen: () ⇒ Unit, onClose: () ⇒ Unit): WebSocket = {

    val ws = Try(new WebSocket(uri)) match {
      case Success(webSocket) ⇒
        logger.info("WebSocket created!")
        webSocket
      case Failure(ex) ⇒
        logger.error("Error during creating WebSocket", ex)
        throw ex
    }

    ws.binaryType = "arraybuffer"

    ws.onopen = { event: Event ⇒
      onOpen()
      logger.info("WebSocket connection opened!")
      event
    }

    ws.onerror = { event: Event ⇒
      //nothing interesting really!
      //val errmsg = s"WebSocket connection failed with event=$event"
      //logger.error(errmsg)
      event.preventDefault()
    }

    ws.onclose = { _: Event ⇒
      onClose()
      logger.info("WebSocket connection closed")
    }

    ws
  }

  def create(): WS = {
    val host = dom.document.location.host
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    val req = write(Protocol.WSNew())
    val uri = s"$wsProtocol://$host/websocket?req=$req"

    val res = new Impl(uri)
    WS.last = Some(res)
    res
  }

  private class Impl(val uri: String) extends WS {
    private val pReady = Promise[Unit]()
    private var handler: Option[Message ⇒ Unit] = None
    private var reconnectCallback: Option[() ⇒ Unit] = None
    private var reconnected = false
    val created: Long = System.currentTimeMillis()
    var connected: Long = -1

    private def onOpen(): Unit = {
      connected = System.currentTimeMillis()
      logger.info(s"ws connected ${new js.Date(connected)}")
      if (reconnected) {
        reconnectCallback.foreach(f ⇒ f())
      } else {
        pReady.complete(Success())
        reconnected = true
      }
    }
    private def onClose(): Unit = {
      connected = -1
      setTimeout(1000) {
        Try {
          ws = init(uri, onOpen, onClose)
          handler.foreach(h ⇒ initHandler(h))
        } recover {
          case _ ⇒ onClose()
        }
      }
    }

    private var ws: WebSocket = init(uri, onOpen, onClose)

    //  var binarySize = 0.0
    //  var textSize = 0.0
    //  var msgs = 0.0
    //  val time = System.currentTimeMillis()
    //  setInterval(1000) {
    //    val ela = System.currentTimeMillis() - time
    //    println(s"ela=$ela, bin=$binarySize, text=$textSize, msgs=$msgs, bin/sec=${binarySize * 1000 / ela}, text/sec=${textSize * 1000 / ela}, msg/sec=${msgs * 1000 / ela}")
    //  }

    def send(msg: String): Unit = if (connected > 0) ws.send(msg)
    def ready: Future[Unit] = pReady.future
    def reconnected(callback: () ⇒ Unit): Unit = {
      reconnectCallback = Some(callback)
    }
    def initHandler(handler: Message ⇒ Unit): Unit = {
      this.handler = Some(handler)
      ws.onmessage = { event: MessageEvent ⇒
        //msgs += 1
        event.data match {
          case arrayBuffer: ArrayBuffer ⇒
            val buffer = TypedArrayBuffer.wrap(arrayBuffer)
            //binarySize += (buffer.limit() - buffer.position())
            handler(BinaryMessage(buffer))
          case data: String ⇒
            //textSize += data.length
            handler(TextMessage(data))
          case _: Blob ⇒
            logger.warn("Get binary message as Blob, ignore it!")
          case other ⇒
            logger.error("Unknown message type: {}", other)
        }
      }
    }
    def sendReady(): Unit = {
      ws.send(write(Protocol.WSReady()))
    }
    def serverLog(msg: String): Unit = {
      val req = Protocol.WSLoggingToServer(msg)
      if (connected > 0) ws.send(write(req))
    }
  }
}
