package plutarch.server.pipelines.spark

import java.util.concurrent.ArrayBlockingQueue
import plutarch.server.pipelines.spark.DebugBuffer.buffer

object DebugBuffer {

  val buffer: ArrayBlockingQueue[(Long, String)] = new ArrayBlockingQueue[(Long, String)](10000)

  def print(i: Int = 10): String = {
    val sb = StringBuilder.newBuilder
    sb.append(s"Log entries count=${buffer.size}").append("\n")
    var j = i
    var elem: (Long, String) = null
    do {
      elem = buffer.poll()
      if (elem != null) {
        val (key, msg) = elem
        sb.append(s"[$key]: $msg").append("\n")
      }
      j -= 1
    } while (j > 0 && elem != null)
    sb.result()
  }

}

trait DebugBuffer {

  def log(msg: Any): Unit = {
    val t = System.currentTimeMillis()
    buffer.offer(t -> msg.toString)
  }

}
