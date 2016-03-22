package tel.fiftythree

import tel.fiftythree.Routes.DSL

import scala.language.higherKinds
import scala.scalajs.js.JSApp

object App extends JSApp {
  def main(): Unit = {
    println("Hello!")

    val loc0 = Location.empty
    val loc1 = Location.fromPathString("/foo/bar")
    val loc2 = Location.fromPathString("/1/3")

    println("loc0: " + RouteMatcher.parse(loc0))
    println("loc1: " + RouteMatcher.parse(loc1))
    println("loc2: " + RouteMatcher.parse(loc2))
  }
}


sealed trait SiteMap
case object Home extends SiteMap
case object FooBar extends SiteMap
final case class NotFound(location: Location) extends SiteMap
final case class Point(dx: Int, dy: Int) extends SiteMap

object SiteMap {
  val _Home: Prism[SiteMap, Unit] =
    Prism.ofMatch(_ => Home, { case Home => () })
  val _FooBar: Prism[SiteMap, Unit] =
    Prism.ofMatch(_ => FooBar, { case FooBar => () })
  val _Point: Prism[SiteMap, (Int, Int)] =
    Prism.ofMatch(x => Point(x._1, x._2), { case Point(x, y) => (x, y) })
  val _NotFound: Prism[SiteMap, Location] =
    Prism.ofMatch(NotFound(_), { case NotFound(loc) => loc })
}

object RouteMatcher extends RouteConfig[SiteMap] {
  def routes[R[_]](dsl: DSL[R]): R[SiteMap] = {
    import dsl._

    ( here                            --> SiteMap._Home
    | some[Int] / some[Int]           --> SiteMap._Point
    | literal("foo") / literal("bar") --> SiteMap._FooBar
    | notFound                        --> SiteMap._NotFound
    )
  }
}
