package fpinscala.lazyness


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

  def append[B >: A](s: Stream[B]):Stream[B] =
    foldRight(s:Stream[B])((elem,acc) => Cons(() => elem,() => acc) )

  def flatMap[B](f: A => Stream[B]) :Stream[B] =
    foldRight(Empty:Stream[B])((elem,acc) => acc.append(f(elem)))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def empty = Empty

  def apply[A](values: A*): Stream[A] = {
    if (values.isEmpty)
      empty
    else
      cons(values.head, apply(values.tail: _*))
  }
}


object main extends App {

  val streamInts = Stream(1, 2, 3, 4, 5)

  println(streamInts)
  println(streamInts.toList)
  println(streamInts.take(3).toList)
  println(streamInts.takeWhile(x => x < 2).toList)
  println(streamInts.takeWhileViaFoldRight(x => x < 2).toList)



}