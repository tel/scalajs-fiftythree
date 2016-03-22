package tel.fiftythree

import tel.fiftythree.Routes.DSL

import scala.language.higherKinds
import scala.scalajs.js.JSApp

object App extends JSApp {
  def main(): Unit = {
    println("Hello!")

    val loc0 = Location.empty
    val loc1 = Location.fromPathString("/foo/bar")

    println("loc0: " + RouteMatcher.parse(loc0))
    println("loc1: " + RouteMatcher.parse(loc1))
  }
}


sealed trait SiteMap
case object Home extends SiteMap
final case class NotFound(location: Location) extends SiteMap

object SiteMap {
  val _Home: Prism[SiteMap, Unit] =
    Prism.ofMatch(_ => Home, { case Home => () })
  val _NotFound: Prism[SiteMap, Location] =
    Prism.ofMatch(NotFound(_), { case NotFound(loc) => loc })
}

object RouteMatcher extends RouteConfig[SiteMap] {
  def routes[R[_]](dsl: DSL[R]): R[SiteMap] = {
    import dsl._

    ( here ~> SiteMap._Home
    | notFound ~> SiteMap._NotFound
    )
  }
}
