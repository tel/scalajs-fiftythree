package tel.fiftythree

import tel.fiftythree.Tuples.Composition

import scala.util.matching.Regex

final case class
  RoutingPrism[A](parses: Location => Result[A],
                  prints: A => (Location => Location)) {

  def parse(loc: Location): Either[RoutingError, A] =
    parses(loc) match {
      case Failure(err) => Left(err)
      case Success(value: A, finalLoc) =>
        if (finalLoc.isEmpty) Right(value) else Left(RoutingError.ExpectedEOL)
    }
  def print(a: A): Location = prints(a)(Location.empty)
}

private sealed trait Result[A] {
  val success: Boolean
  val failure: Boolean = !success
  def map[B](f: A => B): Result[B]
  def flatMapish[B](f: (A, Location) => Result[B]): Result[B]
  def toEither: Either[RoutingError, A]
}
private final case class Failure[A](error: RoutingError) extends Result[A] {
  val success = false
  def map[B](f: A => B) = copy(error)
  def flatMapish[B](f: (A, Location) => Result[B]) = copy(error)
  lazy val toEither = Left(error)
}
private final case class Success[A](value: A, location: Location) extends
  Result[A] {
  val success = true
  def map[B](f: A => B) = copy(value = f(value))
  def flatMapish[B](f: (A, Location) => Result[B]) = f(value, location)
  lazy val toEither = Right(value)
}

private object Result {
  /** Success biased */
  def apply[A](value: A, location: Location): Result[A] =
    Success(value, location)
}

object RoutingPrism {


  object Core extends Routes.Core[RoutingPrism] {

    def map[A, B](f: A => B, g: B => A)(r: RoutingPrism[A]): RoutingPrism[B] = {
      r.copy(
        parses = (loc: Location) => r.parses(loc) map f,
        prints = g andThen r.prints
      )
    }

    def unit[A](a: A): RoutingPrism[A] =
      RoutingPrism(
        parses = Result(a, _),
        prints = (_: A) => identity
      )

    override def pairFlat[A, B]
      (ra: RoutingPrism[A], rb: RoutingPrism[B])
      (implicit c: Composition[A, B]): RoutingPrism[c.C] = {

      RoutingPrism(
        // We can't write this as an locally bound function because of
        // something about not being able to pass a function in with a
        // "dependent" (path-dependent) type.
        parses = (loc0: Location) =>
          ra parses loc0 flatMapish {
            case (a, loc1) => rb parses loc1 flatMapish {
              case (b, loc2) => Result(c.smash(a, b), loc2)
            }
          },

        // We define things locally here as well just for parallelism with
        // `parses`.
        prints = (x: c.C) => rb.prints(c._2(x)) andThen ra.prints(c._1(x))
      )
    }
  }

  object DSL extends Routes.DSL[RoutingPrism] {

    override def literal(repr: String): RoutingPrism[Unit] = {

      def parsesLiteral(loc: Location): Result[Unit] =
        loc.uncons match {
          case None => Failure(RoutingError.UnexpectedEOL)
          case Some((seg, newLoc: Location)) =>
            if (repr == seg)
              Success((), newLoc)
            else
              Failure(RoutingError.ExpectedSegment(repr))
        }

      def printsLiteral(a: Unit)(loc: Location): Location =
        loc.cons(repr)

      RoutingPrism(
        parses = parsesLiteral,
        prints = printsLiteral
      )

    }

    def regex(pattern: Regex): RoutingPrism[String] = {

      def parsesRegex(loc: Location): Result[String] =
        loc.uncons match {
          case None => Failure(RoutingError.UnexpectedEOL)
          case Some((str, newLoc)) => str match {
            case pattern(_*) => Success(str, newLoc)
            case _ => {
              val reason = """does not match /%s/""".format(pattern.toString())
              Failure(RoutingError.NoParse(
                found = str,
                reason = Some(reason)
              ))
            }
          }
        }

      /**
        * We ASSUME that the string that passes back this way would have
        * matched the regex. This invariant is out of our hands.
        */
      def printsRegex(a: String)(loc: Location): Location =
        loc.cons(a)

      RoutingPrism(
        parses = parsesRegex,
        prints = printsRegex
      )
    }

    val core: Routes.Core[RoutingPrism] = Core

    def represented[A](rep: Representation[A]): RoutingPrism[A] = {

      def parsesRepr(loc: Location): Result[A] =
        loc.uncons match {
          case None => Failure(RoutingError.UnexpectedEOL)
          case Some((seg, newLoc)) => rep.parse(seg) match {
            case Left(error) =>
              Failure(RoutingError.NoParse(found = seg, reason = Some(error)))
            case Right(value) =>
              Success(value, newLoc)
          }
        }

      RoutingPrism(
        parses = parsesRepr,
        prints = a => _.cons(rep.print(a))
      )
    }

    def * : RoutingPrism[List[String]] = {

      def parsesRest(loc: Location): Result[List[String]] =
        Success(loc.segments, loc.copy(segments = List()))
      def printsRest(a: List[String])(loc: Location): Location =
        loc.copy(segments = a)

      RoutingPrism(
        parses = parsesRest,
        prints = printsRest
      )
    }
  }

}

