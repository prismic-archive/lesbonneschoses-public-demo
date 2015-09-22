import play.api._
import play.api.mvc._

import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._

import controllers._

object Global extends GlobalSettings {

  override def onRouteRequest(request: RequestHeader) = {
    Helpers.prismicRepository(request).map { _ =>
      super.onRouteRequest(request)
    }.getOrElse {
      Some(Action(_ => Results.Redirect("https://prismic.io")))
    }
  }

  override def onHandlerNotFound(request: RequestHeader) = {
    Prismic.buildContext(request).map { api =>
      controllers.Application.PageNotFound(api)
    }
  }

  override def onError(request: RequestHeader, e: Throwable) = {
    Future.successful(Results.InternalServerError("Unexpected error"))
  }

}
