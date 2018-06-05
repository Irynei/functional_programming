case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](args: A*): Stream[A] =
    if (args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))
}

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case _ => None
    case Cons(h, t) => Some(h())
  }

  def toListRecursive: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }
  }

  def toList: List[A] = {
    def move(stream: Stream[A], acc: List[A]): List[A] = stream match  {
      case Cons(h, t) => move(t(), h()::acc)
      case _ => acc
    }
    move(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def map[B](p: A => B): Stream[B] = this match {
    case Cons(h, t) => Stream.cons(p(h()), t().map(p))
    case _ => Stream.empty
  }

  def filter(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().filter(p))
    case Cons(h, t) if !p(h()) => t().filter(p)
    case _ => Stream.empty
  }

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

}

def isPrime(x: Int): Boolean = {
  def step(value: Int): Boolean = {
      if (value <= 1) true
      else x % value != 0 && step(value - 1)
    }
  step(x - 1)
}


val stream = Stream(1, 2, 3, 4, 5, 6, 7, 8)
stream.from(1).filter(isPrime).take(10).toList