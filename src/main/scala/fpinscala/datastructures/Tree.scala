package fpinscala.datastructures


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //EXERCISE 25: Write a function size that counts the number of nodes in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

}

object main extends App {


  val tree = Branch(Branch(Leaf("leaf"), Leaf("leaf")), Leaf("leaf"))

  println(Tree.size(tree))
}
