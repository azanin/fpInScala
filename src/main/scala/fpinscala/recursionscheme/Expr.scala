package fpinscala.recursionscheme

import scalaz._

sealed trait Expr[+A]

case class Index[A](target: A, idx: A) extends Expr[A]

case class Literal(intVal: Int) extends Expr[Nothing]

case class Ident(name: String) extends Expr[Nothing]

case class Unary[A](op: String, target: A) extends Expr[A]

case class Binary[A](lhs: A, op: String, rhs: A) extends Expr[A]

case class Call[A](func: A, args: Seq[A]) extends Expr[A]

case class Paren[A](target: A) extends Expr[A]


object Expr {

  val exprFunctor = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
      case Index(x, y) => Index(f(x), f(y))
      case Literal(x) => Literal(x)
      case Ident(name) => Ident(name)
      case Unary(op, targer) => Unary(op, f(targer))
      case Binary(lhs, op, rhs) => Binary(f(lhs), op, f(rhs))
      case Call(func, args) => Call(f(func), args.map(f))
      case Paren(target) => Paren(f(target))
    }
  }


  def countNodes: Term.Algebra[Expr, Int] = {
    case Index(x, y) => x + y + 1
    case Literal(_) => 1
    case Ident(_) => 1
    case Unary(_, target) => target + 1
    case Binary(lhs, _, rhs) => lhs + rhs + 1
    case Call(func, args) => args.sum + func + 1
    case Paren(target) => target + 1
  }

  /* def bottomUp(term: Term[Expr])(fn: Term[Expr] => Term[Expr])(functor: Functor[Expr]): Term[Expr] = {
     val expr: Expr[Term[Expr]] = term.out
     fn(Term(functor.map(expr)(t => bottomUp(t)(fn)(functor))))
   }*/
}

case class Term[F[_]](out: F[Term[F]])

object Term {

  type Algebra[F[_], A] = F[A] => A

  def bottomUp[F[_]](fn: Term[F] => Term[F])(implicit functor: Functor[F]): Term[F] => Term[F] = { term =>
    val expr: F[Term[F]] = term.out
    fn(Term(functor.map(expr)(bottomUp(fn)(functor))))
  }


  def bottomUpArrow[F[_]](fn: Term[F] => Term[F])(implicit functor: Functor[F]): Term[F] => Term[F] = {
    import scalaz.std.function._
    import scalaz.syntax.arrow._

    val out: Term[F] => F[Term[F]] = _.out
    val fmap: F[Term[F]] => F[Term[F]] = {
      functor.map(_)(bottomUpArrow(fn)(functor))
    }

    val in: F[Term[F]] => Term[F] = Term(_)

    out >>> fmap >>> in >>> fn
  }

/*
  def mistery[F[_], A](fn: F[A] => A)(functor: Functor[F]): Term[F] => A = {
    import scalaz.std.function._
    import scalaz.syntax.arrow._

    val out: Term[F] => F[Term[F]] = _.out
    val fmap: F[Term[F]] => F[A] = {
      functor.map(_)(mistery(fn)(functor))
    }

    out >>> fmap >>> fn
  }*/


  def cata[F[_], A](algebra: Algebra[F, A])(implicit functor: Functor[F]): Term[F] => A = {
    import scalaz.std.function._
    import scalaz.syntax.arrow._

    val out: Term[F] => F[Term[F]] = _.out
    val fmap: F[Term[F]] => F[A] = {
      functor.map(_)(cata(algebra)(functor))
    }

    out >>> fmap >>> algebra
  }

}


