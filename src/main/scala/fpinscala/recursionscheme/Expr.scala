package fpinscala.recursionscheme

import scalaz._

sealed trait Expr[A]

case class Index[A](x: A, y: A) extends Expr[A]

object Expr {

  val exprFunctor = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
      case Index(x, y) => Index(f(x),f(y))
    }
  }


  def bottomUp(term: Term[Expr])(fn: Term[Expr] => Term[Expr])(functor: Functor[Expr]): Term[Expr] = {
    val expr: Expr[Term[Expr]] = term.out
    fn(Term(functor.map(expr)(t => bottomUp(t)(fn)(functor))))
  }
}

case class Term[F[_]](out: F[Term[F]])

object Term {

  def bottomUp[F[_]](fn: Term[F] => Term[F])(functor: Functor[F]): Term[F] => Term[F] = { term =>
    val expr: F[Term[F]] = term.out
    fn(Term(functor.map(expr)(bottomUp(fn)(functor))))
  }



  def bottomUpArrow[F[_]](fn: Term[F] => Term[F])(functor: Functor[F]): Term[F] => Term[F] = {
    import scalaz.std.function._
    import scalaz.syntax.arrow._

    val out:Term[F] => F[Term[F]] = _.out
    val fmap:F[Term[F]] => F[Term[F]] = {
      functor.map(_)(bottomUpArrow(fn)(functor))
    }

    val in: F[Term[F]] => Term[F] = Term(_)


    out >>> fmap  >>> in >>> fn
  }

}


