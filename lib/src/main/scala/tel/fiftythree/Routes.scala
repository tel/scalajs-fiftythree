package tel.fiftythree

import tel.fiftythree.Tuples.Composition

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex

object Routes {

  /**
    * A Representation is essentially `(String => Either[Error, A], A => String)`
    * such that parse is a retract of print, e.g. `parse(print(x)) = x`. It
    * says that some subset of `String` corresponds exactly to a set of
    * values `A`.
    */
  type Representation[A] = Prism[String, A]

  /**
    * A set of standard representations available in the `Routes` DSL.
    */
  trait Representations {
    import scala.util.control.Exception._

    implicit val stringHasLiteralRepresentation: Representation[String] =
      Prism[String, String](identity, Some(_))

    implicit val intHasRepresentation: Representation[Int] =
      Prism(
        inject = (x: Int) => x.toString,
        view = repr => {
          val catcher: Catch[Int] = catching(classOf[NumberFormatException])
          catcher opt Integer.valueOf(repr)
        }
      )
  }

  trait DSL[Router[_]] extends Representations {

    /** Parses successfully only if we're at the end of the path */
    def here: Router[Unit]

    /**
      * Router which always succeeds returning whatever part of the
      * `Location` remains
      */
    def notFound: Router[Location]

    /** Matches an exact string */
    def literal(repr: String): Router[Unit] =
      core.map[String, Unit](_ => (), _ => repr)(regex(repr.r))

    /** Matches any string that passes a regex filter */
    def regex(pattern: Regex): Router[String]

    /**
      * Convenient interface for `represented` where we use an implicit
      * `Representation` value. Same as `represented[A](implicitly)`.
      */
    def some[A: Prism.Representation]: Router[A] = represented(implicitly)

    /**
      * Provided a `Representation` of some type as a String, we check to see
      * if the next segment of the path matches.
      */
    def represented[A](repr: Prism.Representation[A]): Router[A]

    /** Matches any non-empty string */
    def string: Router[String] = some

    /** Matches all remaining segments */
    def * : Router[List[String]]

    /** Core semantic combinators */
    val core: Core[Router]

    implicit class DslInfixOperations[A](ra: Router[A]) {
      def /[B](rb: Router[B])(implicit c: Composition[A, B]): Router[c.C] =
        // NOTE: This shows as an error in IntelliJ, but is actually correct :(
        core.pairFlat(ra, rb)(c)

      def |(other: Router[A]): Router[A] = core.alt(ra, other)

      def -->[S](prism: Prism[S, A]): Router[S] =
        core.mapIf((a: A) => Some(prism.inject(a)), prism.view)(ra)
    }

  }

  /**
    * Core exterior properties of a Router.
    *
    * The primary namespace to use is `DSL`. These are separated out (though
    * still available) since they form the basis of operation but don't help
    * with the presentation of the DSL.
    */
  trait Core[Router[_]] {

    /**
      * Slightly more general than `map`, `mapIf` lets either direction of
      * the mapping fail.
      */
    def mapIf[A, B](f: A => Option[B], g: B => Option[A])(r: Router[A]): Router[B]

    /**
      * Given a transform and its inverse (really, retraction) we can transform
      * a Router producing one type to a different router.
      */
    def map[A, B](f: A => B, g: B => A)(r: Router[A]): Router[B] =
      mapIf((a: A) => Some(f(a)), (b: B) => Some(g(b)))(r)

    /**
      * Inject a value into a router without examining or consuming the
      * location.
      */
    def unit[A](a: A): Router[A]

    /**
      * Execute two parses sequentially and return both results. Unlike
      * `pair`, `pairFlat` uses an implicit `Composition` to try to smash
      * tuples together "flatly".
      */
    def pairFlat[A, B](ra: Router[A], rb: Router[B])(implicit c: Composition[A, B]): Router[c.C] =
      map[(A, B), c.C](x => c.smash(x._1, x._2), x => (c._1(x), c._2(x)))(pair(ra, rb))

    /**
      * Execute two parses sequentially and return both results as a tuple.
      * Unlike `pairFlat` this does not do any extra work to try to flatten
      * tuples. Can be considered a type-specialized version of `pairFlat`.
      */
    def pair[A, B](ra: Router[A], rb: Router[B]): Router[(A, B)] = pairFlat(ra, rb)

    /**
      * Select sequentially from two `Router`s. If `r1` succeeds then `r2` is
      * not tried. If `r1` fails then this is the same as `r2`.
      */
    def alt[A](r1: Router[A], r2: => Router[A]): Router[A]

  }
}
