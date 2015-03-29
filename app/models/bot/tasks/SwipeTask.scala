package models.bot.tasks

import akka.actor._
import models.bot.BotLog
import play.api.Logger
import services.{FacialAnalysisService, TinderBot, TinderService}
import utils.face.FacialDetection
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits._
import java.util.Date
import utils.tinder.TinderApi
import utils.tinder.model._
import models.bot.logic.{LikeAction, Liker}

/**
 * Worker task that processes recommendations.
 *
 * @note This is one of the most complex tasks in the bot since it analyzes many features of a recommendation.
 */
class SwipeTask(val xAuthToken: String, val tinderBot: ActorRef, val rec: RecommendedUser, val autoLike: Boolean=false) extends TaskActor {

  override def preStart() = {
    Logger.debug("[tinderbot] Starting new swipe task.")
    self ! "tick"
  }

  private val tinderApi = new TinderApi(Some(xAuthToken))

  private val user = TinderService.fetchSession(xAuthToken).get

  private def dislikeUser(reason: String) = {
    tinderApi.swipeNegative(rec._id).map { result =>
      result match {
        case Left(e) =>
          Logger.error("[tinderbot] Swipe task had an error on Tinder: "+e.error)

        case Right(r) =>
          val log = BotLog(
            System.currentTimeMillis(),
            "swipe_dislike",
            "Disliked %s because: %s.".format(rec.name, reason),
            Some(rec._id),
            Some(rec.photos.head.url)
          )
          TinderBot.writeLog(user.user._id, log)
          Logger.info("[tinderbot] Disliked user "+rec._id)
          Logger.debug("[tinderbot] User was disliked because: "+reason)
      }
    }
    user.user
  }

  private def likeUser(reason: String) = {
    tinderApi.swipePositive(rec._id).map { result =>
      result match {
        case Left(e) =>
          Logger.error("[tinderbot] Swipe task had an error on Tinder: "+e.error)

        case Right(r) =>
          val log = BotLog(
            System.currentTimeMillis(),
            "swipe_like",
            "Liked %s because: %s.".format(rec.name, reason),
            Some(rec._id),
            Some(rec.photos.head.url)
          )
          TinderBot.writeLog(user.user._id, log)
          if(r==null || r==None) Logger.info("[tinderbot] Liked user "+rec._id)
          else Logger.info("[tinderbot] Matched with user "+rec._id)
      }
    }
  }

  private def ignoreUser(reason: String) = {
    val log = BotLog(
      System.currentTimeMillis(),
      "swipe_ignore",
      "Ignored %s because: %s.".format(rec.name, reason),
      Some(rec._id),
      Some(rec.photos.head.url)
    )
    TinderBot.writeLog(user.user._id, log)
    Logger.info("[tinderbot] Ignored recommended user "+rec._id)
  }



  def receive = {
    case "tick" =>
      // some initial criteria

      val lastSeenAgo = {
        val now = System.currentTimeMillis
        val lastSeen = tinderApi.ISO8601.parse(rec.ping_time).getTime
        now - lastSeen
      }

      /*
      Here we assess a recommended user. The strategy is to eliminate users who
      don't meet certain criteria.
       */

      val outcome = Liker.mvpLike(rec, lastSeenAgo) //, autoLike, user.user._id)

      outcome._1 match {
        case LikeAction.Like => likeUser(outcome._2)
        case LikeAction.Dislike => dislikeUser(outcome._2)
        case LikeAction.Ignore => ignoreUser(outcome._2)
      }

      // make sure we properly shut down this actor
      self ! PoisonPill

    // someone is sending invalid messages
    case e: Any =>
      Logger.error("[tinderbot] Task received an unknown message")
      Logger.error("[tinderbot] Received: \n %s" format e.toString)

  }

}
