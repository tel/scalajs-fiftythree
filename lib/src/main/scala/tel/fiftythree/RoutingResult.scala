package tel.fiftythree

/**
  * A private result type isomorphic to `Either[RoutingError, (A, Location)]`
  * but a little nicer to work with. Only shows up in the implementation of
  * `Router`
  *
  * Created by tel on 3/21/16.
  */
private[fiftythree] sealed trait RoutingResult[A] {
  val success: Boolean
  val failure: Boolean = !success
  def map[B](f: A => B): RoutingResult[B]
  def mapIf[B](f: A => Option[B]): RoutingResult[B]
  def flatMapish[B](f: (A, Location) => RoutingResult[B]): RoutingResult[B]
  def toEither: Either[RoutingError, A]
  def orElse(other: => RoutingResult[A]): RoutingResult[A]
}

private[fiftythree] final case class Failure[A](error: RoutingError) extends RoutingResult[A] {
  val success = false
  def map[B](f: A => B) = copy(error)
  def mapIf[B](f: A => Option[B]) = copy(error)
  def flatMapish[B](f: (A, Location) => RoutingResult[B]) = copy(error)
  lazy val toEither = Left(error)
  def orElse(other: => RoutingResult[A]): RoutingResult[A] = other
}

private[fiftythree] final case class Success[A](value: A, location: Location) extends RoutingResult[A] {
  val success = true
  def map[B](f: A => B) = copy(value = f(value))
  def mapIf[B](f: A => Option[B]) = f(value) match {
    case None => Failure(RoutingError.Custom("mapIf failure"))
    case Some(b) => Success(b, location)
  }
  def flatMapish[B](f: (A, Location) => RoutingResult[B]) = f(value, location)
  lazy val toEither = Right(value)
  def orElse(other: => RoutingResult[A]): RoutingResult[A] = this
}
