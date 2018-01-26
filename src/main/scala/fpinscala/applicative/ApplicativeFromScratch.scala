package fpinscala.applicative

sealed trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}


sealed trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fa: F[A])(fab: F[A => B]): F[B]
  def unit[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(fa)(unit(f))
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val curriedF: A => B => C = f.curried
    val fbc: F[B => C] = map(fa)(curriedF)
    apply(fb)(fbc)
  }


/*
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f:(A,B,C) => D): F[D] = {
    val fcd: F[C => D] = map2(fa, fb)((a, b) => f(a, b, _))
    apply(fc)(fcd)
  }*/

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curried: A => B => C => D = f.curried
    apply(fc)(apply(fb)(apply(fa)(unit(curried))))
  }

  def applyViaMap2[A, B](fa: F[A])(fab: F[A => B]): F[B] = {
    map2(fa, fab)((a:A, ab: A => B) => ab(a))
  }

}


object Applicative {

  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] =
    (oab, oa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
}

