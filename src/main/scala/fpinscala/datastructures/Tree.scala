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

  //EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  //EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r)) // or 1 + (depth(l) max depth(r))
  }

}

object main extends App {


  val tree = Branch(Branch(Leaf("leaf"), Leaf("leaf")), Leaf("leaf"))

  println(Tree.size(tree))
  print(Tree.depth(tree))
}
