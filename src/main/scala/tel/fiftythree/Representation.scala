package tel.fiftythree

/**
  * A Representation is essentially `(String => Either[Error, A], A => String)`
  * such that parse is a retract of print, e.g. `parse(print(x)) = x`.
  */
trait Representation[A] {
  def parse(repr: String): Either[String, A]
  def print(value: A): String
}

/**
  * This object stores implicit Representations for various common types.
  */
object Representation {
  import scala.util.control.Exception._

  implicit val stringHasLiteralRepresentation = new Representation[String] {
    def parse(repr: String) = Right(repr)
    def print(value: String) = value
  }

  implicit val intHasRepresentation = new Representation[Int] {
    def parse(repr: String): Either[String, Int] = {
      val catcher: Catch[Int] = catching(classOf[NumberFormatException])
      val almost: Either[Throwable, Int] = catcher either Integer.valueOf(repr)
      almost.left.map(_.toString())
    }
    def print(value: Int): String = value.toString()
  }
}

