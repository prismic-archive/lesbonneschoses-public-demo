package controllers

import play.api._
import play.api.mvc._

import org.joda.time._

import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent._

import Play.current

import io.prismic._

object Helpers {

  val DomainRegex = """(lesbonneschoses([^.]*))[.]prismic[.]me""".r

  def prismicRepository(implicit request: RequestHeader): Option[String] = {
    request.domain match {
      case DomainRegex(repository, _) => Some(repository)
      case _ => None
    }
  }

}

/**
 * Main controller for the Website.
 *
 * It uses some helpers provided by `controllers.Prismic`
 */
class Application extends Controller {

  import Prismic._

  def brokenLink = Prismic.action { implicit request =>
    Future.successful(Application.PageNotFound)
  }

  // -- Home page

  def index = Prismic.action { implicit request =>
    for {
      products <- ctx.api.forms("products").ref(ctx.ref).submit()
      featured <- ctx.api.forms("featured").ref(ctx.ref).submit()
    } yield {
      Ok(views.html.index(products.results, featured.results))
    }
  }

  // -- About us

  def about = Prismic.action { implicit request =>
    for {
      maybePage <- getBookmark("about")
    } yield {
      maybePage.map(page => Ok(views.html.about(page))).getOrElse(Application.PageNotFound)
    }
  }

  // -- Jobs

  def jobs = Prismic.action { implicit request =>
    for {
      maybePage <- getBookmark("jobs")
      jobs <- ctx.api.forms("jobs").ref(ctx.ref).submit()
    } yield {
      maybePage.map(page => Ok(views.html.jobs(page, jobs.results))).getOrElse(Application.PageNotFound)
    }
  }

  def jobDetail(id: String, slug: String) = Prismic.action { implicit request =>
    for {
      maybePage <- getBookmark("jobs")
      maybeJob <- getDocument(id)
    } yield {
      checkSlug(maybeJob, slug) {
        case Left(newSlug) => MovedPermanently(routes.Application.jobDetail(id, newSlug).url)
        case Right(job) => maybePage.map { page =>
          Ok(views.html.jobDetail(page, job))
        }.getOrElse(Application.PageNotFound)
      }
    }
  }

  // -- Stores

  def stores = Prismic.action { implicit request =>
    for {
      maybePage <- getBookmark("stores")
      stores <- ctx.api.forms("stores").ref(ctx.ref).submit()
    } yield {
      maybePage.map(page => Ok(views.html.stores(page, stores.results))).getOrElse(Application.PageNotFound)
    }
  }

  def storeDetail(id: String, slug: String) = Prismic.action { implicit request =>
    for {
      maybeStore <- getDocument(id)
    } yield {
      checkSlug(maybeStore, slug) {
        case Left(newSlug) => MovedPermanently(routes.Application.storeDetail(id, newSlug).url)
        case Right(store)  => Ok(views.html.storeDetail(store))
      }
    }
  }

  // -- Products Selections

  def selectionDetail(id: String, slug: String) = Prismic.action { implicit request =>
    for {
      maybeSelection <- getDocument(id)
      products <- getDocuments(maybeSelection.map(_.getAll("selection.product").collect {
        case Fragment.DocumentLink(id, _, "product", _, _, _, false) => id
      }).getOrElse(Nil): _*)
    } yield {
      checkSlug(maybeSelection, slug) {
        case Left(newSlug)    => MovedPermanently(routes.Application.selectionDetail(id, newSlug).url)
        case Right(selection) => Ok(views.html.selectionDetail(selection, products))
      }
    }
  }

  // -- Blog

  def blog(maybeCategory: Option[String]) = Prismic.action { implicit request =>
    for {
      posts <- maybeCategory.map(
        category => ctx.api.forms("blog").query(s"""[[:d = at(my.blog-post.category, "$category")]]""").orderings("[my.blog-post.date desc]")
      ).getOrElse(ctx.api.forms("blog").orderings("[my.blog-post.date desc]")).ref(ctx.ref).submit()
    } yield {
      Ok(views.html.posts(posts.results))
    }
  }

  def blogPost(id: String, slug: String) = Prismic.action { implicit request =>
    for {
      maybePost <- getDocument(id)
      relatedProducts <- getDocuments(maybePost.map(_.getAll("blog-post.relatedproduct").collect {
        case Fragment.DocumentLink(id, _, "product", _, _, _, false) => id
      }).getOrElse(Nil): _*)
      relatedPosts <- getDocuments(maybePost.map(_.getAll("blog-post.relatedpost").collect {
        case Fragment.DocumentLink(id, _, "blog-post", _, _, _, false) => id
      }).getOrElse(Nil): _*)
    } yield {
      checkSlug(maybePost, slug) {
        case Left(newSlug) => MovedPermanently(routes.Application.blogPost(id, newSlug).url)
        case Right(post)   => Ok(views.html.postDetail(post, relatedProducts, relatedPosts))
      }
    }
  }

  // -- Products

  def products = Prismic.action { implicit request =>
    for {
      products <- ctx.api.forms("products").ref(ctx.ref).submit()
    } yield {
      Ok(views.html.products(products.results))
    }
  }

  def productDetail(id: String, slug: String) = Prismic.action { implicit request =>
    for {
      maybeProduct <- getDocument(id)
      relatedProducts <- getDocuments(maybeProduct.map(_.getAll("product.related").collect {
        case Fragment.DocumentLink(id, _, "product", _, _, _, false) => id
      }).getOrElse(Nil): _*)
    } yield {
      checkSlug(maybeProduct, slug) {
        case Left(newSlug)  => MovedPermanently(routes.Application.productDetail(id, newSlug).url)
        case Right(product) => Ok(views.html.productDetail(product, relatedProducts))
      }
    }
  }

  def productsByFlavour(flavour: String) = Prismic.action { implicit request =>
    for {
      products <- ctx.api.forms("everything").query(s"""[[:d = at(my.product.flavour, "$flavour")]]""").ref(ctx.ref).submit()
    } yield {
      if (products.results.isEmpty) {
        Application.PageNotFound
      }
      else {
        Ok(views.html.productsByFlavour(flavour, products.results))
      }
    }
  }

  // -- Search

  def search(query: Option[String]) = Prismic.action { implicit request =>
    query.map(_.trim).filterNot(_.isEmpty).map { q =>
      for {
        products <- ctx.api.forms("everything").query(s"""[[:d = any(document.type, ["product", "selection"])][:d = fulltext(document, "$q")]]""").ref(ctx.ref).submit()
        others <- ctx.api.forms("everything").query(s"""[[:d = any(document.type, ["article", "blog-post", "job-offer", "store"])][:d = fulltext(document, "$q")]]""").ref(ctx.ref).submit()
      } yield {
        Ok(views.html.search(query, products.results, others.results))
      }
    }.getOrElse {
      Future.successful(Ok(views.html.search()))
    }
  }

  // -- Preview Action

  def preview(token: String) = Prismic.action { implicit req =>
    ctx.api.previewSession(token, ctx.linkResolver, routes.Application.index.url).map { redirectUrl =>
      Redirect(redirectUrl).withCookies(Cookie(io.prismic.Prismic.previewCookie, token, path = "/", maxAge = Some(30 * 60), httpOnly = false))
    }
  }

}

object Application {

  val BlogCategories = List(
    "Announcements",
    "Do it yourself",
    "Behind the scenes"
  )

  val ProductCategories = collection.immutable.ListMap(
    "Macaron" -> "Macarons",
    "Cupcake" -> "Cup Cakes",
    "Pie" -> "Little Pies"
  )

  // -- Page not found

  def PageNotFound(implicit ctx: Prismic.Context) = Results.NotFound(views.html.pageNotFound())

  // -- Resolve links to documents

  def linkResolver(api: Api)(implicit request: RequestHeader) = DocumentLinkResolver(api) {

    // For "Bookmarked" documents that use a special page
    case (link@Fragment.DocumentLink, Some("about")) => routes.Application.about.absoluteURL()
    case (link@Fragment.DocumentLink, Some("jobs")) => routes.Application.jobs.absoluteURL()
    case (link@Fragment.DocumentLink, Some("stores")) => routes.Application.stores.absoluteURL()

    // Store documents
    case (Fragment.DocumentLink(id, _, "store", _, slug, _, false), _) => routes.Application.storeDetail(id, slug).absoluteURL()

    // Any product
    case (Fragment.DocumentLink(id, _, "product", _, slug, _, false), _) => routes.Application.productDetail(id, slug).absoluteURL()

    // Product selection
    case (Fragment.DocumentLink(id, _, "selection", _, slug, _, false), _) => routes.Application.selectionDetail(id, slug).absoluteURL()

    // Job offers
    case (Fragment.DocumentLink(id, _, "job-offer", _, slug, _, false), _) => routes.Application.jobDetail(id, slug).absoluteURL()

    // Blog
    case (Fragment.DocumentLink(id, _, "blog-post", _, slug, _, false), _) => routes.Application.blogPost(id, slug).absoluteURL()

    case anyOtherLink => routes.Application.brokenLink.absoluteURL()
  }

}
