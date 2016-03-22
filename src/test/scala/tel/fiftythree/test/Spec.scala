package tel.fiftythree.test

import tel.fiftythree._
import utest._

import scala.language.higherKinds

object RoutesTests extends TestSuite {

  trait WithRoutes[A] {
    def apply[R[_]](dsl: Routes.DSL[R]): R[A]
  }

  def reify[A](withRoutes: WithRoutes[A]): RoutingPrism[A] =
    withRoutes(RoutingPrism.DSL)

  def tests = this {

    val emptyLoc = Location.fromPathString("/")
    val fooLoc = Location.fromPathString("/foo")
    val barLoc = Location.fromPathString("/bar")
    val foobarLoc = Location.fromPathString("/foo/bar")
    val foobarbazLoc = Location.fromPathString("/foo/bar/baz")

    "Locations parse properly" - {

      "/" - {
        assert(emptyLoc.segments == List())
      }

      "/foo" - {
        assert(fooLoc.segments == List("foo"))
      }

      "/bar" - {
        assert(barLoc.segments == List("bar"))
      }

      "/foo/bar" - {
        assert(foobarLoc.segments == List("foo", "bar"))
      }

      "/foo/bar/baz" - {
        assert(foobarbazLoc.segments == List("foo", "bar", "baz"))
      }

    }

    "basic parsing: /foo/bar" - {

      import RoutingError._

      val rp = reify(new WithRoutes[Unit] {
        def apply[R[_]](dsl: Routes.DSL[R]) = {
          import dsl._

          literal("foo") / literal("bar")
        }
      })


      "/ is too short" - {
        assertMatch(rp.parse(emptyLoc)) {
          case Left(UnexpectedEOL) =>
        }
      }
      "/bar starts out wrong" - {
        val parse = rp.parse(barLoc)
        val result = Left(ExpectedSegment("foo"))
        assert(parse == result)
      }
      "/foo ends too soon" - {
        val parse = rp.parse(fooLoc)
        val result = Left(UnexpectedEOL)
        assert(parse == result)
      }
      "/foo/bar/baz is too long" - {
        val parse = rp.parse(foobarbazLoc)
        val result = Left(ExpectedEOL)
        assert(parse == result)
      }
      "/foo/bar is just right" - {
        assert(rp.parse(foobarLoc) == Right(()))
      }

    }

  }

}

