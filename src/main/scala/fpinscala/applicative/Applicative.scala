package fpinscala.applicative

sealed trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

sealed trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    // val a = map(fa)(a => f(a,_))
    val fbc: F[B => C] = map(fa)(a => f.curried(a))
    apply(fb)(fbc)
  }

  /*
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val curried: A => B => C = f.curried
      val fabc: F[A => B => C] = unit(curried)
      val fbc: F[B => C] = apply(fa)(fabc)
      apply(fb)(fbc)
    }*/

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curried: A => B => C => D = f.curried
    val fabcd: F[A => B => C => D] = unit(curried)
    val fbcd: F[B => C => D] = apply(fa)(fabcd)
    val fcd: F[C => D] = apply(fb)(fbcd)
    apply(fc)(fcd)
  }


  /*def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f:(A,B,C) => D): F[D] = {
    val fcd: F[C => D] = map2(fa, fb)((a, b) => f(a, b, _))
    apply(fc)(fcd)
  }*/

  def applyInTermsOfMap2[A, B](fa: F[A])(fab: F[A => B]): F[B] = {
    map2(fa, fab)((a: A, ab: A => B) => ab(a))
  }


  /** *
    * The action of apply is similar to the familiar map. Both are a kind of function
    * application in a context, but there's a very specific difference. In the case of
    * apply, the function being applied might be affected by the context. For example,
    * if the second argument to apply is None in the case of Option then there is no
    * function at all. But in the case of map, the function must exist independently of the
    *context. This is easy to see if we rearrange the type signature of map a little and
    * compare it to apply.
    * The only difference is the F around the function argument type. The apply
    * function is strictly more powerful, since it can have that added F effect. It makes
    * sense that we can implement map in terms of apply, but not the other way
    * around.
    */
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(fa)(unit(f))
  }

  def apply[A, B](fa: F[A])(fab: F[A => B]): F[B]

  def unit[A](a: A): F[A]

  def compose[G[_]](appG: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]]})#f] {
      override def apply[A, B](fga: F[G[A]])(fgab: F[G[A => B]]): F[G[B]] = {
        val x: F[G[A] => G[B]]= self.map(fgab)(appG.flip)
        self.apply(fga)(x)
      }
      override def unit[A](a: A): F[G[A]] = self.unit(appG.unit(a))
    }
  }

  def flip[A, B](gab: F[A => B]): F[A] => F[B] = ga => apply(ga)(gab)

  def product[G[_]](appG: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      override def unit[A](a: A): (F[A], G[A]) = {
        val fa: F[A] = self.unit(a)
        val ga: G[A] = appG.unit(a)
        (fa, ga)
      }

      override def apply[A, B](fga: (F[A], G[A]))(fgab: (F[A => B], G[A => B])) = {
        val fb: F[B] = self.apply(fga._1)(fgab._1)
        val fc = appG.apply(fga._2)(fgab._2)
        (fb, fc)
      }
    }
  }


}

object Applicative {


  /** *You can see that this method combines two Options. But one of them
    * contains a function (unless it is None of course). The action of apply is to apply
    * the function inside one argument to the value inside the other. This is the origin of
    * the name "applicative". This operation is sometimes called idiomatic function
    * application since it occurs within some idiom or context. ***/
  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] =
    (oab, oa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }

}
