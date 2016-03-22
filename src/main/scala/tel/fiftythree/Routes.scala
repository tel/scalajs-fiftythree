package tel.fiftythree

import tel.fiftythree.Tuples.Composition

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex

object Routes {

  trait DSL[Router[_]] {

    /** Matches an exact string */
    def literal(repr: String): Router[Unit] =
      core.map[String, Unit](_ => (), _ => repr)(regex(repr.r))

    /** Matches any string that passes a regex filter */
    def regex(pattern: Regex): Router[String]

    /**
      * Convenient interface for `represented` where we use an implicit
      * `Representation` value. Same as `represented[A](implicitly)`.
      */
    def some[A: Representation]: Router[A] = represented(implicitly)

    /**
      * Provided a `Representation` of some type as a String, we check to see
      * if the next segment of the path matches.
      */
    def represented[A](repr: Representation[A]): Router[A]

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
      * Given a transform and its inverse (really, retraction) we can transform
      * a Router producing one type to a different router.
      */
    def map[A, B](f: A => B, g: B => A)(r: Router[A]): Router[B]

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

  }
}
