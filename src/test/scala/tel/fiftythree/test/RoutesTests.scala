package tel.fiftythree.test

import tel.fiftythree._
import utest._

import scala.language.higherKinds

object RoutesTests extends TestSuite {

  val FooBarMatcher = new RouteConfig[Unit] {
    def routes[R[_]](dsl: Routes.DSL[R]): R[Unit] = {
      import dsl._
      literal("foo") / literal("bar")
    }
  }

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

      "/ is too short" - {
        assertMatch(FooBarMatcher.parse(emptyLoc)) {
          case Left(UnexpectedEOL) =>
        }
      }
      "/bar starts out wrong" - {
        val parse = FooBarMatcher.parse(barLoc)
        val result = Left(ExpectedSegment("foo"))
        assert(parse == result)
      }
      "/foo ends too soon" - {
        val parse = FooBarMatcher.parse(fooLoc)
        val result = Left(UnexpectedEOL)
        assert(parse == result)
      }
      "/foo/bar/baz is too long" - {
        val parse = FooBarMatcher.parse(foobarbazLoc)
        val result = Left(ExpectedEOL)
        assert(parse == result)
      }
      "/foo/bar is just right" - {
        assert(FooBarMatcher.parse(foobarLoc) == Right(()))
      }

    }

    "FooBarMatcher printing: /foo/bar" - {
      // Since our result type is just `Unit` we've lost too much information
      // to do interesting printing results.
      val print: Option[Location] = FooBarMatcher.print(())
      val result: Option[Location] = Some(Location.fromPathString("/foo/bar"))
      assert(print == result)
    }

  }

}

