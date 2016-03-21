package tel.fiftythree

/**
  * Copied, essentially, from japgolly/scalajs-react
  */
object Tuples {

  trait Composition[A, B] {
    type C
    val _1: C => A
    val _2: C => B
    val smash: (A, B) => C
  }

  trait Composition_Priority_0 {
    implicit def ***[A, B] = Composition[A, B, (A, B)](_._1, _._2, (_, _))
  }
  trait Composition_Priority_1 extends Composition_Priority_0 {
    implicit def T8[A, B, C, D, E, F, G, H] = Composition[(A, B, C, D, E, F, G), H, (A, B, C, D, E, F, G, H)](r => (r._1, r._2, r._3, r._4, r._5, r._6, r._7), _._8, (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r))
    implicit def T7[A, B, C, D, E, F, G] = Composition[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)](r => (r._1, r._2, r._3, r._4, r._5, r._6), _._7, (l, r) => (l._1, l._2, l._3, l._4, l._5, l._6, r))
    implicit def T6[A, B, C, D, E, F] = Composition[(A, B, C, D, E), F, (A, B, C, D, E, F)](r => (r._1, r._2, r._3, r._4, r._5), _._6, (l, r) => (l._1, l._2, l._3, l._4, l._5, r))
    implicit def T5[A, B, C, D, E] = Composition[(A, B, C, D), E, (A, B, C, D, E)](r => (r._1, r._2, r._3, r._4), _._5, (l, r) => (l._1, l._2, l._3, l._4, r))
    implicit def T4[A, B, C, D] = Composition[(A, B, C), D, (A, B, C, D)](r => (r._1, r._2, r._3), _._4, (l, r) => (l._1, l._2, l._3, r))
    implicit def T3[A, B, C] = Composition[(A, B), C, (A, B, C)](r => (r._1, r._2), _._3, (l, r) => (l._1, l._2, r))
  }
  trait Composition_Priority_2 extends Composition_Priority_1 {
    implicit def _toA[A] = Composition[Unit, A, A](_ => (), identity, (_, a) => a)
    implicit def Ato_[A] = Composition[A, Unit, A](identity, _ => (), (a, _) => a)
  }
  object Composition extends Composition_Priority_2 {
    implicit def _to_ = Composition[Unit, Unit, Unit](_ => (), _ => (), (_, _) => ())

    type Aux[A, B, O] = Composition[A, B] {type C = O}

    def apply[A, B, O](a: O => A, b: O => B, c: (A, B) => O): Aux[A, B, O] =
      new Composition[A, B] {
        override type C = O
        val _1 = a
        val _2 = b
        val smash = c
      }
  }

}

