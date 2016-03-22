package tel.fiftythree

/**
  * A `Matcher` is a value which can print and parse `Location` values
  *
  * Created by tel on 3/21/16.
  */
trait Matcher[A] {
  def parse(loc: Location): Either[RoutingError, A]
  def print(a: A): Option[Location]
}
