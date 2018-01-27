package fpinscala.recursionscheme

object Main extends App {

  val ten = Term[Expr](Literal(10))
  val five = Term[Expr](Literal(5))
  val add = Term[Expr](Ident("add"))
  val call = Term[Expr](Call(func = add, Seq(ten, five)))


  val transformLiterals: Term[Expr] => Term[Expr] = {
    term => term.out match {
      case Literal(10) => Term[Expr](Literal(5))
      case Ident(name) => Term[Expr](Ident(name.toUpperCase))
      case _ => term
    }
  }

  println(call.out)
  println(Term.bottomUp[Expr](transformLiterals)(Expr.exprFunctor)(call).out)
}
