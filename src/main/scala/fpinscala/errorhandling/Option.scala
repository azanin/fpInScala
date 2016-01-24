package fpinscala.errorhandling


sealed trait Option[+A] {
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = flatMap((a) => if (f(a)) Some(a) else None)

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }


}


case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {


  //EXERCISE 3: Write a generic function map2, that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(v1 => b.map(v2 => f(v1, v2)))


  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head.flatMap(v => sequence(tail).map(l => v :: l))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }


}


object main extends App {

  val listOption = List(Some(2), Some(3))

  println(Option.sequence(listOption))
}