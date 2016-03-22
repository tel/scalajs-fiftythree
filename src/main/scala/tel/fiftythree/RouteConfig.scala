package tel.fiftythree

import scala.language.higherKinds

/**
  * Use `RouteConfig` to construct a `Matcher` value. Define an
  * `RouteConfig` by providing an implementation of `Routes` and then use the
  * `print` and `parse` implementations to interpret a `Location` value.
  *
  * Created by tel on 3/21/16.
  */
trait RouteConfig[A] extends Matcher[A] {

  /**
    * Given a Routes DSL, define your routing logic. This function is defined
    * polymorphically over `R` meaning that you must construct the value
    * using only functions from the `dsl` implementation---the type `R`
    * remains abstract.
    */
  def routes[R[_]](dsl: Routes.DSL[R]): R[A]

  /**
    * Produce a `Matcher` object based on the `routes` defined.
    */
  val matcher: Matcher[A] =
    // Obviously we use the implementation we have---the `Router`.
    //
    // Less obviously, if we wanted to do an optimization pass we could place
    // it here by going through some other type (perhaps a representation of the
    // Routes AST) and optimizing it *before* constructing a `Router`.
    routes(Router.DSL)

  def parse(loc: Location): Either[RoutingError, A] = matcher.parse(loc)

  def print(a: A): Option[Location] = matcher.print(a)
}
