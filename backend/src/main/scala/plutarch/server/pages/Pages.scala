package plutarch.server.pages

import java.nio.charset.StandardCharsets
import scalatags.Text.all._
import scalatags.Text.TypedTag
import scalatags.Text.tags2.title
import buildinfo.BuildInfo

object Pages {
  val tmsp: String = System.currentTimeMillis().toString
  private val utf8: String = StandardCharsets.UTF_8.displayName
  private val jsApp: String = s"/web/js/$tmsp/frontend-${if (BuildInfo.fullOpt) "" else "fast"}opt.js"

  def defaultHead(pageId: String, pageTitle: String): TypedTag[String] =
    head(id := pageId)(
      meta(charset := utf8),
      BuildInfo.stylesheets.map { ref ⇒
        link(rel := "stylesheet", href := s"/web/lib/$ref")
      },
      title(pageTitle),
      script(src := jsApp))

  def htmlWithDocType(typedTags: TypedTag[_]*): String = "<!DOCTYPE html>" + html(typedTags)

  val notFound: String = {
    val id = "page-not-found-id"
    val mkHead = defaultHead(id, "Pages not found")
    val mkBody = p("We are sorry, the page you requested doesn't exist")
    htmlWithDocType(mkHead, mkBody)
  }

  val welcome: String = {
    val id = "welcome-page-id"
    val myHead = defaultHead(id, "Hello there")
    val myBody = body()
    htmlWithDocType(myHead, myBody)
  }

}