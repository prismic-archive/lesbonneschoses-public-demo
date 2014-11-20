package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.ws._

import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent._

import Play.current

import io.prismic._

/**
 * This Prismic object contains several helpers that make it easier
 * to build your application using both Prismic.io and Play:
 *
 * It reads some configuration from the application.conf file.
 *
 * The debug and error messages emitted by the Scala Kit are redirected
 * to the Play application Logger.
 *
 * It provides an "Action builder" that help to create actions that will query
 * a prismic.io repository.
 */
object Prismic extends Controller {

  // -- Cache to use (default to keep 200 JSON responses in a LRU cache)
  private val Cache = new BuiltInCache(200)

  // -- Write debug and error messages to the Play `prismic` logger (check the configuration in application.conf)
  private val Logger = (level: Symbol, message: String) => level match {
    case 'DEBUG => play.api.Logger("prismic").debug(message)
    case 'ERROR => play.api.Logger("prismic").error(message)
    case _      => play.api.Logger("prismic").info(message)
  }

  // Helper method to read the Play application configuration
  private def config(key: String) = Play.configuration.getString(key).getOrElse(sys.error(s"Missing configuration [$key]"))

  // -- Define a `Prismic request` that contain both the original request and the Prismic call context
  case class Request[A](request: play.api.mvc.Request[A], ctx: Context) extends WrappedRequest(request)

  // -- A Prismic context that help to keep the reference to useful primisc.io contextual data
  case class Context(api: Api, ref: String, accessToken: Option[String], linkResolver: DocumentLinkResolver) {
    def maybeRef = Option(ref).filterNot(_ == api.master.ref)
    def hasPrivilegedAccess = accessToken.isDefined
  }

  object Context {
    implicit def fromRequest(implicit req: Request[_]): Context = req.ctx
  }

  // -- Build a Prismic context
  def buildContext(implicit request: RequestHeader) = {
    apiHome(None) map { api =>
      val ref = { 
        // First check if there is a preview token in a cookie
        request.cookies.get(io.prismic.Prismic.previewCookie).map(_.value)
      }.orElse {
        // Otherwise check if user must see a specific experiment variation
        request.cookies.get(io.prismic.Prismic.experimentsCookie).map(_.value).flatMap(api.experiments.refFromCookie)
      }.getOrElse {
        // Otherwise use the master ref 
        api.master.ref
      }
      Context(api, ref, None, Application.linkResolver(api)(request))
    }
  }

  // -- Action builder
  def bodyAction[A](bodyParser: BodyParser[A])(block: Prismic.Request[A] => Future[Result]) = Action.async(bodyParser) { implicit request =>
    (
      for {
        ctx <- buildContext
        result <- block(Request(request, ctx))
      } yield result
    ).recover {
      case e: ApiError => InternalServerError(e.getMessage).discardingCookies(
        DiscardingCookie(name = io.prismic.Prismic.previewCookie, path = "/")
      )
    }
  }

  // -- Alternative action builder for the default body parser
  def action(block: Prismic.Request[AnyContent] => Future[Result]): Action[AnyContent] = bodyAction(parse.anyContent)(block)

  // -- Retrieve the Prismic Context from a request handled by an built using Prismic.action
  def ctx(implicit req: Request[_]) = req.ctx

  // -- Fetch the API entry document
  def apiHome(accessToken: Option[String] = None)(implicit rh: RequestHeader) = {
    Helpers.prismicRepository.map {
      case "lesbonneschoses" => Api.get(config("prismic.api"), accessToken = accessToken, cache = Cache, logger = Logger)
      case repository => Api.get(s"https://$repository.prismic.io/api", accessToken = accessToken, cache = Cache, logger = Logger)
    }.getOrElse {
      sys.error("How can we get there without a proper URL?")
    }
  }

  // -- Helper: Retrieve a single document by Id
  def getDocument(id: String)(implicit ctx: Prismic.Context): Future[Option[Document]] = {
    for {
      documents <- ctx.api.forms("everything").query(s"""[[:d = at(document.id, "$id")]]""").ref(ctx.ref).submit()
    } yield {
      documents.results.headOption
    }
  }

  // -- Helper: Retrieve several documents by Id
  def getDocuments(ids: String*)(implicit ctx: Prismic.Context): Future[Seq[Document]] = {
    ids match {
      case Nil => Future.successful(Nil)
      case ids => ctx.api.forms("everything")
        .query(s"""[[:d = any(document.id, ${ids.mkString("[\"", "\",\"", "\"]")})]]""").ref(ctx.ref).submit() map (_.results)
    }
  }

  // -- Helper: Retrieve a single document from its bookmark
  def getBookmark(bookmark: String)(implicit ctx: Prismic.Context): Future[Option[Document]] = {
    ctx.api.bookmarks.get(bookmark).map(id => getDocument(id)).getOrElse(Future.successful(None))
  }

  // -- Helper: Check if the slug is valid and redirect to the most recent version id needed
  def checkSlug(document: Option[Document], slug: String)(callback: Either[String, Document] => Result)(implicit r: Prismic.Request[_]) = {
    document.collect {
      case document if document.slug == slug         => callback(Right(document))
      case document if document.slugs.contains(slug) => callback(Left(document.slug))
    }.getOrElse {
      Application.PageNotFound
    }
  }

  // -- Preview Action
  def preview(token: String) = Prismic.action { implicit req =>
    ctx.api.previewSession(token, ctx.linkResolver, routes.Application.index.url).map { redirectUrl =>
      Redirect(redirectUrl).withCookies(Cookie(io.prismic.Prismic.previewCookie, token, path = "/", maxAge = Some(30 * 60), httpOnly = false))
    }
  }

}
