package tel.fiftythree

/**
  * Representation of the current browser location. For compatibility with
  * Hash Routing this does not include a fragment component (after all, the
  * whole location might be a fragment).
  */
case class Location(segments: List[String] = List(),
                    query: Map[String, Option[String]] = Map()) {

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
  def fromPathString(s: String): Location =
    Location(segments = s.split('/') filter(_.nonEmpty) toList)

  def empty: Location = Location()
}
