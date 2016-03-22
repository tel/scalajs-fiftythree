package tel.fiftythree

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

/**
  * Representation of the current browser location. For compatibility with
  * Hash Routing this does not include a fragment component (after all, the
  * whole location might be a fragment).
  */
case class Location(segments: List[String] = List(),
                    query: Map[String, Option[String]] = Map()) {

  /**
    * A `Location` is empty if it has no (remaining) segments. This
    * defines a "complete" parse.
    */
  def isEmpty: Boolean = segments.isEmpty

  /**
    * "Un"-conses the top path segment off the location returning an updated
    * location. May fail if we're at the root path.
    */
  def uncons: Option[(String, Location)] = segments match {
    case Nil => None
    case (a :: rest) => Some(a, copy(segments = rest))
  }

  /**
    * Cons'es a new path segment onto the location, pushing it to the
    * "leftmost" or "highest" position.
    */
  def cons(segment: String): Location =
    copy(segments = segment :: segments)

  /**
    * Update the query map with a new key and (optional) value.
    */
  def updated(key: String, value: Option[String]): Location =
    copy(query = query.updated(key, value))

  /**
    * Sets the value at a key in the query. This differs from `updated` since it
    * does not offer you the ability to have a key present with no value.
    */
  def set(key: String, value: String): Location =
    updated(key, Some(value))

  /**
    * Sets a key to be present in the query. If that key already has a value
    * then no change occurs.
    */
  def declare(key: String): Location =
    copy(query = query.updated(key, query.getOrElse(key, None)))

  /**
    * Sets a key to be missing entirely from the query.
    */
  def remove(key: String): Location =
    copy(query = query - key)

  /**
    * Looks up a value in the query component of a location. The outer Option
    * indicates the existence or non-existence of the key in the query. The
    * inner Option indicates whether a value was provided.
    */
  def get(key: String): Option[Option[String]] =
    query.get(key)
}

object Location {

  /* TODO: Do we need to URL decode the pieces here? Probably! */
  def fromPathString(s: String): Location =
    Location(segments = s.split('/').filter(_.nonEmpty).toList)

  /**
    * The empty location has no path and no search.
    */
  def empty: Location = Location()

  /**
    * Decode a `Location` from the current `window.location` value
    */
  def currentLocation(): Location = {
    val theLocation: Js.WindowLocation = Js.Window.location
    Location.fromPathString(theLocation.pathname).copy(
      query = Js.parseSearch(theLocation.search)
    )
  }

  /**
    * Javascript interop types and functions
    */
  private object Js {

    @JSName("window")
    @js.native
    object Window extends js.Object {
      val location: WindowLocation = js.native
    }

    trait WindowLocation {
      val href: String
      val protocol: String
      val host: String
      val hostname: String
      val port: String
      val pathname: String
      val search: String
      val hash: String
      val username: String
      val password: String
      val origin: String
    }

    /* TODO: Do we need to URL decode the pieces here? Probably! */
    def parseSearch(search: String): Map[String, Option[String]] = {
      val withoutQMark = search.drop(1)
      withoutQMark.split("&").map { seg =>
        val pieces = seg.split("=")
        if (pieces.length == 1)
          (pieces(0), None)
        else
          (pieces(0), Some(pieces(1)))
      }.toMap[String, Option[String]]
    }
  }

}
