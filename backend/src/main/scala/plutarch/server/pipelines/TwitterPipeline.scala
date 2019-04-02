/*
 *    Copyright (c) 2019 Pythian and Valentin Nikotin
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package plutarch.server.pipelines

import scala.util.{ Failure, Success, Try }
import java.text.SimpleDateFormat
import java.util.{ Date, Locale }

import twitter4j.{ RawStreamListener, TwitterStream, TwitterStreamFactory }
import twitter4j.auth.AccessToken
import plutarch.server.data.metricManager.MetricManager
import plutarch.server.ws.WebSocketFlowCoordinator
import org.json4s._
import org.json4s.jackson.JsonMethods._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.metrics.Metric
import plutarch.shared.data.Aggregations.{ Avg, Count, Max, Min, Sum }
import plutarch.shared.data.metrics.Conf

import scala.concurrent.duration._
import scala.language.postfixOps

object TwitterPipeline {

  case class Hashtag(text: String)
  case class UserMention(screen_name: String)
  case class Entities(hashtags: List[Hashtag], user_mentions: List[UserMention])
  case class Tweet(id: Long, created_at: Date, entities: Entities)

  case class TMC(tweet: Tweet)
  case object Tick

  implicit val formats: DefaultFormats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss ZZZZZ yyyy", Locale.ENGLISH)
  }
}

class TwitterPipeline(
    webSocketFlowCoordinator: WebSocketFlowCoordinator,
    metricManager:            MetricManager,
    system:                   ActorSystem) extends LazyLogging {
  import system.dispatcher
  import TwitterPipeline._

  val mconf: Conf = Conf(
    name = "TwitterHashtags",
    step = 1000,
    scales = Seq(1, 5, 10, 30, 60, 300, 600, 1800, 3600, 3600 * 6, 3600 * 12, 3600 * 24),
    //scales = Seq(1, 3, 6, 30, 60, 180, 360, 360 * 6, 360 * 12, 360 * 24),
    aggregations = Seq(Sum, Min, Max, Count, Avg),
    withTotal = true)

  val metric: Metric = metricManager.getOrCreate(mconf)
  val step: Long = mconf.step

  implicit val timeout: Timeout = Timeout(step millis)

  private val conf = system.settings.config
  private val topN = conf.getInt("app.twitter.topN")
  private val minCount = conf.getInt("app.twitter.minCount")

  def publish(t: Long, data: Seq[(String, Double)], silent: Boolean = false): Unit = {
    metric.add(t, data)
    if (!silent) webSocketFlowCoordinator.inform(metric.name)
  }

  private val actor = system.actorOf(Props(new Actor {
    private val counter = collection.mutable.Map.empty[String, Int]
    def receive: Receive = {
      case TMC(tweet) ⇒
        //logger.info(s"tags=${tweet.entities.hashtags.mkString(",")}")
        for (tag ← tweet.entities.hashtags) {
          counter(tag.text) = counter.getOrElse(tag.text, 0) + 1
        }
      case Tick ⇒
        val tmsp = step * (System.currentTimeMillis() / step)
        val data: List[(String, Double)] =
          counter
            .toList
            .filter(data ⇒ data._2 > minCount)
            .sortBy(-_._2)
            .take(topN)
            .map(x ⇒ (x._1, x._2.toDouble))
        //logger.info(s"tick with data  $tmsp, $data")
        publish(tmsp, data)
        counter.clear()
    }
  }))

  private def rawListener(): RawStreamListener = new RawStreamListener {
    def onMessage(rawString: String): Unit = Try {
      //logger.info(rawString)
      if (rawString != null && rawString.trim != "") {
        val json = parse(rawString)
        json.findField {
          case JField("created_at", _) ⇒ true
          case _                       ⇒ false
        } match {
          case Some(_) ⇒
            val tweet = json.extract[Tweet]
            if (tweet.entities.hashtags.nonEmpty) {
              actor ! TMC(tweet)
            }
          case None ⇒
        }
      }
    } match {
      case Success(_) ⇒
      case Failure(ex) ⇒
        ex.printStackTrace()
    }
    def onException(ex: Exception): Unit = {
      // todo: handle error and restart stream N times within M seconds
      logger.error(s"Twitter listener error", ex)
    }
  }

  private val factory = new TwitterStreamFactory()
  private val consumerKey = conf.getString("app.twitter.consumerKey")
  private val consumerSecret = conf.getString("app.twitter.consumerSecret")
  private val accessToken = conf.getString("app.twitter.accessToken")
  private val accessTokenSecret = conf.getString("app.twitter.accessTokenSecret")
  private val stream: TwitterStream = factory.getInstance

  stream.setOAuthConsumer(consumerKey, consumerSecret)
  stream.setOAuthAccessToken(new AccessToken(accessToken, accessTokenSecret))
  stream.addListener(rawListener())
  stream.sample()

  def when: Int = {
    val now = System.currentTimeMillis()
    val tmsp = step * (now / step)
    if (now - tmsp < 500) 444
    else if (tmsp + step - now < 500) 777
    else 0
  }

  system.scheduler.schedule(when.millis, step.millis) {
    actor ! Tick
  }

  def terminate(): Unit = {
    stream.shutdown()
  }

}