package models.bot.logic

import models.bot.logic.LikeAction.LikeAction
import models.bot.tasks.recommendation
import play.api.Logger
import utils.face.FacialDetection
import utils.tinder.model.{Photo, RecommendedUser}

/**
 * Created by dan on 3/27/15.
 */

/*
 Preference (like) logic
 */


object LikeAction extends Enumeration {
  type LikeAction = Value
  val Like, Dislike, Ignore = Value
}

object Liker {

 def pickyLike(rec : RecommendedUser, lastSeenAgo : Long, autoLike : Boolean,ownUserId : String)
   : (LikeAction, String) = {

   import LikeAction._

   if(rec.photos.size==2 && rec.bio=="") (Dislike, "sparse photos, no bio")
   else if (rec.photos.size==1) (Dislike, "sparse photos")
   else if (lastSeenAgo > (day*3)) (Dislike, "hasn't been active for %s days".format((lastSeenAgo/day)))
   else if (!photoCriteria(rec.photos, rec)) (Dislike, "failed photo criteria")
   else if (rec.bio.matches("no.{0,15}hookups")) (Dislike, "claiming friendship only")
   else if (autoLike) (Like, "auto-liked")
   else {
     recommendation.FacialRecommendation.makeComparison(ownUserId, rec._id, rec.photos) match {
       case Some(true) => (Like, "face matched positive recommendation criteria")
       case Some(false) => (Dislike, "face did not match recommendation criteria")
       case None => (Ignore, "not enough data for decision")
     }
   }
 }

  /* lowest standards liking, no criteria */
  def mvpLike(rec : RecommendedUser, lastSeenAgo : Long)= {
    (rec.photos.size >= 1 && lastSeenAgo < day * 5, "has pictures and has been around")
  }

  private def photoCriteria(photos: List[Photo], rec : RecommendedUser): Boolean = {
    val facesPerPhoto: List[Int] = photos.map { photo =>
      FacialDetection(photo.url).countFaces
    }

    Logger.debug("[tinderbot] Number of faces for user %s: %s".format(rec._id, facesPerPhoto.toString))

    if(facesPerPhoto.find(p => p==1) == None) false // no singular photo of themselves
    else if(facesPerPhoto.sum==0) false // can't find a face in any photo
    else true
  }

  val day = 86400000L
}
