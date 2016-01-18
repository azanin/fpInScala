package fpinscala.datastructures

import scala.annotation.tailrec

object List {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }


    //EXERCISE 10: foldRight is not tail-recursive and will StackOverflow for large lists.
    // Convince yourself that this is the case, then write another general list-recursion function, foldLeft that is tail-recursive,
    def foldLeft[T, B](l: List[T], z: B)(f: (B, T) => B): B = {
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    }

    //EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight? How about the other way around?

    def foldRightViaFoldLeft[T, B](ts: List[T], z: B)(f: (T, B) => B): B = {
      foldLeft(reverse(ts), z)((b, t) => f(t, b))
    }

    def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)


    //EXERCISE 14: Implement append in terms of either foldLeft or foldRight.
    def appendViaFoldRight[T](ls: List[T], rs: List[T]): List[T] = {
      foldRight(ls, rs)(Cons(_, _))
    }

    //EXERCISE 15 (hard): Write a function that concatenates a list of lists into a single list.
    // Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
    def concat[T](l: List[List[T]]): List[T] = {
      foldRight(l, Nil: List[T])(append)
    }


    //EXERCISE 11: Write sum, product, and a function to compute the length of a list using foldLeft.

    def sumFoldLeft[T](ts: List[Int]): Int = {
      foldLeft(ts, 0)(_ + _)
    }

    def productFoldLeft[T](ts: List[Int]): Int = {
      foldLeft(ts, 0)(_ * _)
    }

    def lengthFoldLeft[T](ts: List[Int]): Int = {
      foldLeft(ts, 0)((index, elem) => index + 1)
    }

    def foldRight[T, B](l: List[T], z: B)(f: (T, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    /*
    EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0 if it encounters a 0.0?
     No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
     which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
     to support early termination.
    */


    /*
    EXERCISE 8: See what happens when you pass Nil and Cons themselves to foldRight, like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
    What do you think this says about the relationship between foldRight and the data constructors of List?

    we get back to the original list
     */


    //EXERCISE 9: Compute the length of a list using foldRight.
    def length[T](ts: List[T]): Int = {
      foldRight(ts, 0)((elem, index) => index + 1)
    }


    def sum2(l: List[Int]) =
      foldRight(l, 0.0)(_ + _)

    def product2(l: List[Double]) =
      foldRight(l, 1.0)(_ * _)


    //EXERCISE 2: Implement the function tail for "removing" the first element of a List. Notice the function takes constant time.
    // What are different choices you could make in your implementation if the List is Nil? We will return to this question in the next chapter.

    def tail[T](ts: List[T]): List[T] = ts match {
      case Nil => sys.error("init of empty list")
      case Cons(x, xs) => xs
    }

    //EXERCISE 3: Generalize tail to the function drop, which removes the first n elements from a list.
    @tailrec
    def drop[T](ts: List[T], n: Int): List[T] =
      if (n > 0)
        ts match {
          case Nil => sys.error("init of empty list")
          case Cons(x, xs) => drop(xs, n - 1)
        }
      else ts


    //EXERCISE 4: Implement dropWhile,10 which removes elements from the List prefix as long as they match a predicate.
    @tailrec
    def dropWhile[T](ts: List[T])(predicate: T => Boolean): List[T] = {
      ts match {
        case Nil => sys.error("init of empty list")
        case Cons(head, tail) =>
          if (!predicate(head))
            dropWhile(ts)(predicate)
          else ts
      }

    }


    //EXERCISE 5: Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
    def setHead[T](ts: List[T], head: T): List[T] = ts match {
      case Nil => Cons(head, List())
      case Cons(x, xs) => Cons(head, xs)
    }

    def init[T](l: List[T]): List[T] =
      l match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }


    //EXERCISE 12: Write a function that returns the reverse of a list (so given List(1,2,3) it returns List(3,2,1))
    def reverse[T](ts: List[T]): List[T] =
      foldLeft(ts, List[T]())((res: List[T], elem: T) => Cons(elem, res))


    //EXERCISE 16: Write a function that transforms a list of integers by adding 1 to each element
    def add1(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((elem: Int, b: List[Int]) => Cons(elem + 1, b))


    //EXERCISE 17: Write a function that turns each value in a List[Double] into a String.
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((h: Double, b: List[String]) => Cons(h.toString, b))


    //EXERCISE 18: Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list.
    def map[A, B](l: List[A])(f: A => B): List[B] = {
      foldRight(l, Nil: List[B]) ((elem: A, b: List[B]) => Cons(f(elem), b))
    }


    //EXERCISE 19: Write a function filter that removes elements from a list unless they satisfy a given predicate.
    def filter[A](l: List[A])(predicate: A => Boolean): List[A] =
      foldRight(l, Nil: List[A])((h: A, t: List[A]) => if (predicate(h)) Cons(h, t) else t)


    //EXERCISE 20: Write a function flatMap, that works like map except that the function given will return a list
    // instead of a single result, and that list should be inserted into the final resulting list.
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      concat(map(l)(f))


    //EXERCISE 21: Can you use flatMap to implement filter?
    def filterViaFlatMap[A](l: List[A])(predicate: A => Boolean): List[A] =
      flatMap(l)((a: A) => if (predicate(a)) List(a) else Nil)

    //EXERCISE 22: Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

    //EXERCISE 23: Generalize the function you just wrote so that it's not specific to integers or addition.
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
      (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }

    }


    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1, 2, 3)

    val total = sum(example)
  }

}
