package tel.fiftythree

import tel.fiftythree.RoutingError.ExpectedEOL
import tel.fiftythree.Tuples.Composition

import scala.util.matching.Regex

private[fiftythree] final case class
  Router[A](parses: Location => RoutingResult[A],
            prints: A => (Location => Option[Location]))
  extends Matcher[A] {

  def parse(loc: Location): Either[RoutingError, A] =
    parses(loc) match {
      case Failure(err) => Left(err)
      case Success(value, finalLoc) =>
        if (finalLoc.isEmpty)
          Right(value)
        else
          Left(RoutingError.ExpectedEOL)
    }

  def print(a: A): Option[Location] = prints(a)(Location.empty)
}


object Router {

  object Core extends Routes.Core[Router] {

    def mapIf[A, B](f: (A) => Option[B], g: (B) => Option[A])(r: Router[A]): Router[B] =
      Router(
        parses = loc => r.parses(loc) mapIf f,
        prints = b => loc => g(b) flatMap (a => r.prints(a)(loc))
      )

    override def map[A, B](f: A => B, g: B => A)(r: Router[A]): Router[B] = {
      r.copy(
        parses = (loc: Location) => r.parses(loc) map f,
        prints = g andThen r.prints
      )
    }

    def unit[A](a: A): Router[A] =
      Router(
        parses = Success(a, _),
        prints = (_: A) => Some(_)
      )

    override def pairFlat[A, B]
      (ra: Router[A], rb: Router[B])
      (implicit c: Composition[A, B]): Router[c.C] = {

      Router(
        // We can't write this as an locally bound function because of
        // something about not being able to pass a function in with a
        // "dependent" (path-dependent) type.
        parses = (loc0: Location) =>
          ra parses loc0 flatMapish {
            case (a, loc1) => rb parses loc1 flatMapish {
              case (b, loc2) => Success(c.smash(a, b), loc2)
            }
          },

        // We define things locally here as well just for parallelism with
        // `parses`.
        prints = (x: c.C) => (loc0: Location) =>
          for {
            loc1 <- rb.prints(c._2(x))(loc0)
            loc2 <- ra.prints(c._1(x))(loc1)
          } yield loc2
      )
    }

    def alt[A](r1: Router[A], r2: => Router[A]): Router[A] = {

      def parsesAlt(loc: Location): RoutingResult[A] = {
        r1.parses(loc) orElse r2.parses(loc)
      }

      def printsAlt(a: A)(loc: Location): Option[Location] =
        r1.prints(a)(loc) orElse r2.prints(a)(loc)

      Router(
        parses = parsesAlt,
        prints = printsAlt
      )
    }

  }

  object DSL extends Routes.DSL[Router] {

    override def literal(repr: String): Router[Unit] = {

      def parsesLiteral(loc: Location): RoutingResult[Unit] =
        loc.uncons match {
          case None => Failure(RoutingError.UnexpectedEOL)
          case Some((seg, newLoc: Location)) =>
            if (repr == seg)
              Success((), newLoc)
            else
              Failure(RoutingError.ExpectedSegment(repr))
        }

      def printsLiteral(a: Unit)(loc: Location): Option[Location] =
        Some(loc.cons(repr))

      Router(
        parses = parsesLiteral,
        prints = printsLiteral
      )

    }

    def regex(pattern: Regex): Router[String] = {

      def parsesRegex(loc: Location): RoutingResult[String] =
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

      def printsRegex(a: String)(loc: Location): Option[Location] =
        a match {
          case pattern(_*) => Some(loc.cons(a))
          case _ => None
        }

      Router(
        parses = parsesRegex,
        prints = printsRegex
      )
    }

    val core: Routes.Core[Router] = Core

    def represented[A](rep: Representation[A]): Router[A] = {

      def parsesRepr(loc: Location): RoutingResult[A] =
        loc.uncons match {
          case None => Failure(RoutingError.UnexpectedEOL)
          case Some((seg, newLoc)) => rep.parse(seg) match {
            case Left(error) =>
              Failure(RoutingError.NoParse(found = seg, reason = Some(error)))
            case Right(value) =>
              Success(value, newLoc)
          }
        }

      Router(
        parses = parsesRepr,
        prints = a => l => Some(l.cons(rep.print(a)))
      )
    }

    def * : Router[List[String]] = {

      def parsesRest(loc: Location): RoutingResult[List[String]] =
        Success(loc.segments, loc.copy(segments = List()))

      def printsRest(a: List[String])(loc: Location): Option[Location] =
        Some(loc.copy(segments = a))

      Router(
        parses = parsesRest,
        prints = printsRest
      )
    }

    def here: Router[Unit] = {

      def parsesHere(loc: Location): RoutingResult[Unit] =
        if (loc.isEmpty)
          Success((), loc)
        else
          Failure(ExpectedEOL)

      def printsHere(a: Unit)(loc: Location): Option[Location] =
        if (loc.isEmpty)
          Some(loc)
        else
          None

      Router(
        parses = parsesHere,
        prints = printsHere
      )
    }

    def notFound: Router[Location] =
      Router(
        parses = loc => Success(loc, Location.empty),
        prints = a => loc =>
          if (loc.isEmpty)
            Some(a)
          else
            None
      )
  }

}

