package tel.fiftythree

sealed trait RoutingError
object RoutingError {

  /** Expecting more path segments, none left */
  case object UnexpectedEOL extends RoutingError

  /** Expecting no further path segments, some left */
  case object ExpectedEOL extends RoutingError

  /**
    * Expected a particular, constant segment. Note that merely *missing* a
    * segment will trigger an `UnexpectedEOL` error.
    */
  case class ExpectedSegment(seg: String) extends RoutingError

  /** Expecting a query parameter at a given key */
  final case class NoKey(key: String) extends RoutingError

  /** Expected to parse a path segment appropriately, couldn't */
  final case class NoParse(found: String, reason: Option[String]) extends RoutingError

  /** Expected... something. Couldn't find it! */
  final case class Custom(reason: String) extends RoutingError
}

