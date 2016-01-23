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

  //EXERCISE 28: Write a function map
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


  //EXERCISE 29: Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  // Reimplement them in terms of this more general function.

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)((l, r) => l max r)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((b, c) => 1 + (b max c))
}

object main extends App {


  val tree = Branch(Branch(Leaf("leaf"), Leaf("leaf")), Leaf("leaf"))

  println(Tree.size(tree))
  println(Tree.depth(tree))
  println(Tree.map(tree)((a) => a.size))
  println(Tree.sizeViaFold(tree) == Tree.size(tree))


}
