package fpinscala.basics

import scala.annotation.tailrec


object Basic {


  //Exercise 1: fibonacci tailrecursive
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) n

    @tailrec
    def tailFib(pos: Int, f1: Int, f2: Int): Int = {
      if (pos == 2)
        f1 + f2
      else
        tailFib(pos - 1, f1 + f2, f1)
    }

    tailFib(n, f1 = 1, f2 = 0)
  }


  //Exercise 2:  Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function.
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    @tailrec
    def tailIsSorted(pos: Int): Boolean = {
      if (pos == as.length - 1)
        return true
      if (!gt(as(pos), as(pos + 1)))
        false
      else tailIsSorted(pos + 1)
    }
    tailIsSorted(0)
  }

  //Exercise 3 Implement partial1.
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    f(a, _)
  }

  //Exercise 4 Let's look at another example, currying, which converts a function of N arguments into a function of one argument that returns another function as its result.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    partial1(_, f)
  }

  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = {
    (x: A) => f(x, _)
  }


  //Exercise 5 (optional): Implement uncurry, which reverses the transformation of curry.
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (x: A, y: B) => f(x)(y)
  }


  //Exercise 6: Implement the higher-order function that composes two functions.
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    f compose (g)
  }


}
