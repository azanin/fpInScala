package fpinscala.lazyness

import fpinscala.lazyness.Stream._

trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
    case Empty => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
    case Empty => Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((head, tail) =>
      if (p(head)) Cons(() => head, () => tail)
      else Empty
    )

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((elem, acc) => Cons(() => f(elem), () => acc))

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s: Stream[B])((elem, acc) => Cons(() => elem, () => acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((elem, acc) => acc.append(f(elem)))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((elem, acc) => if (p(elem)) Cons(() => elem, () => acc) else Empty: Stream[A])

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this, n) {
    case (Cons(h, t), n) => if (n > 0) Some(h(), (t(), n - 1)) else None
    case _ => None
  }

  def zip[B](stream: Stream[B]): Stream[(A, B)] = unfold((this, stream)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipViaZipWith[B](stream: Stream[B]): Stream[(A, B)] = zipWith(stream)((_, _))

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, stream) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipWithAll[B, C](stream: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold(this, stream) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (Empty, t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
  }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]


object Stream {
  def empty = Empty

  def apply[A](values: A*): Stream[A] = {
    if (values.isEmpty)
      empty
    else
      cons(values.head, apply(values.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val as: Stream[A] = cons(a, as)
    as
  }

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def rec(f0: Int, f1: Int): Stream[Int] = cons(f0, rec(f1, f0 + f1))
    rec(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h: A, s: S)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibsViaUnfold() = unfold((0, 1)) { case (f0: Int, f1: Int) => Some(f0, (f1, f0 + f1)) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zip(s2).forAll(x => x._1 == x._2)

}


object main extends App {

  val streamInts = Stream(1, 2, 3, 4, 5)

  println(streamInts)
  println(streamInts.toList)
  println(streamInts.take(3).toList)
  println(streamInts.takeWhile(x => x < 2).toList)
  println(streamInts.takeWhileViaFoldRight(x => x < 2).toList)
  println(streamInts.map(x => x * 2).toList)
  println(streamInts.filter(x => x % 2 != 0).toList)


  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesViaUnfold: Stream[Int] = Stream.unfold(1)(_ => Some(1, 1))

  println(ones.take(5).toList)
  println(onesViaUnfold.take(5).toList)
  //println(ones.takeWhile(x => x == 1).toList) //stackOverflow
  println(ones.forAll(_ != 1))
  println(Stream.constant(2).take(15).toList)
  println(Stream.from(5).take(5).toList)
  println(Stream.fibs().take(10).toList)
  println(Stream.fibsViaUnfold().take(10).toList)
  println(Stream.fromViaUnfold(5).take(5).toList)
  println(Stream.constantViaUnfold(2).take(15).toList)
  println(streamInts.mapViaUnfold(x => x * 2).toList)
  println(streamInts.takeViaUnfold(5).toList)


  val streamB = unfold(1)(x => Some(x, x + 1))
  println(streamInts.zipViaZipWith(streamB).take(5).toList)
  println(streamInts.zipWithAll(streamB)((a, b) => a.getOrElse(0) + b.getOrElse(0)).take(5).toList)
  println(Stream.startsWith(streamB, streamInts))


}