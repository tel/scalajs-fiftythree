package tel.fiftythree


trait Prism[S, A] {
  def inject: A => S
  def view: S => Option[A]
}

object Prism {
  def apply[S, A](inject: A => S, view: S => Option[A]): Prism[S, A] = {
    val injectArg = inject
    val viewArg = view

    new Prism[S, A] {
      val inject = injectArg
      val view = viewArg
    }
  }

  def ofMatch[S, A](inject: A => S, view: PartialFunction[S, A]): Prism[S, A] =
    apply(inject, view.lift)

  def tup2[S, A, B](inject: (A, B) => S, view: S => Option[(A, B)]): Prism[S, (A, B)] =
    apply[S, (A, B)](x => inject(x._1, x._2), view)

  def tup3[S, A, B, C](inject: (A, B, C) => S, view: S => Option[(A, B, C)]): Prism[S, (A, B, C)] =
    apply[S, (A, B, C)](x => inject(x._1, x._2, x._3), view)


  /**
    * A Representation is essentially `(String => Either[Error, A], A => String)`
    * such that parse is a retract of print, e.g. `parse(print(x)) = x`. It
    * says that some subset of `String` corresponds exactly to a set of
    * values `A`.
    */
  type Representation[A] = Prism[String, A]

  object Representation {
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
}
