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

    'Locations {

      'EmptyLoc - {
        assert(emptyLoc.segments == List())
      }

      'FooLoc - {
        assert(fooLoc.segments == List("foo"))
      }

      'BarLoc - {
        assert(barLoc.segments == List("bar"))
      }

      'FooBarLoc - {
        assert(foobarLoc.segments == List("foo", "bar"))
      }

      'FooBarBazLoc - {
        assert(foobarbazLoc.segments == List("foo", "bar", "baz"))
      }

    }

    'Parsing {

      import RoutingError._

      val rp = reify(new WithRoutes[Unit] {
        def apply[R[_]](dsl: Routes.DSL[R]) = {
          import dsl._

          literal("foo") / literal("bar")
        }
      })


      'NoSegmentFailure - {
        assertMatch(rp.parse(emptyLoc)) {
          case Left(UnexpectedEOL) =>
        }
      }
      'WrongSegmentFailure - {
        val parse = rp.parse(barLoc)
        val result = Left(ExpectedSegment("foo"))
        assert(parse == result)
      }
      'IncompleteParse - {
        val parse = rp.parse(fooLoc)
        val result = Left(UnexpectedEOL)
        assert(parse == result)
      }
      'EarlyMatchShouldFail - {
        val parse = rp.parse(foobarbazLoc)
        val result = Left(ExpectedEOL)
        assert(parse == result)
      }
      'SegmentMatch - {
        assert(rp.parse(foobarLoc) == Right(()))
      }

    }

  }

}

