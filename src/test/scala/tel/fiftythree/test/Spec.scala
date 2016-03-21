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

    }

    'Parsing {

      import RoutingError._

      val rp = reify(new WithRoutes[Unit] {
        def apply[R[_]](dsl: Routes.DSL[R]) = {
          import dsl._

          literal("foo")
        }
      })


      'NoSegmentFailure - {
        assertMatch(rp.parse(emptyLoc)) {
          case Left(UnexpectedEOL) =>
        }
      }
      'WrongSegmentFailure - {
        assertMatch(rp.parse(barLoc)) {
          case Left(ExpectedSegment("foo")) =>
        }
      }
      'SegmentMatch - {
        assertMatch(rp.parse(fooLoc)) {
          case Right(()) =>
        }
      }

    }

  }

}

