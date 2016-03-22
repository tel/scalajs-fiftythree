# Fifty Three

A routing library.

## Example

```scala
sealed trait SiteMap
case object Home extends SiteMap
case object FooBar extends SiteMap
case class NotFound(location: Location) extends SiteMap
case class Point(dx: Int, dy: Int) extends SiteMap


// Build some Prisms so we can do printing and parsing of locations

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


// Construct an object extending RouteConfig. It'll inherit implementations of
// `print` and `parse`.

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
```
