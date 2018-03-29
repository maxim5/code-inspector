package arglang.lib

import pimps._

trait Stream[+A] {
  import Stream._
  def uncons: Option[(A,Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append (constant(None))
    val b = s2 map (Some(_)) append (constant(None))
    unfold((a, b)) {
      case (s1,s2) if s1.isEmpty && s2.isEmpty => None
      case (s1,s2) => {
        val (h1,t1) = s1.uncons.get
        val (h2,t2) = s2.uncons.get
        Some((f(h1,h2), (t1,t2)))
      }
    }
  }

  def toList: List[A] =
    uncons match {
      case Some((x, xs)) => x :: xs.toList
      case None => List.empty[A]
    }

  def take(n: Int): Stream[A] = {
    uncons match {
      case Some((x, xs)) if n > 0 => cons(x, xs.take(n - 1))
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    uncons match {
      case Some((x, xs)) if p(x) => cons(x, xs.takeWhile(p))
      case _ => empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    uncons match {
      case Some((x, xs)) => f(x,xs.foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((x, a) => cons(f(x),a))
  }

  def map2[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this,b))(s => s._1.uncons.map2(s._2.uncons)(
      (aa,bb) => (f(aa._1,bb._1), aa._2 -> bb._2)))

  }


  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((x, a) => if(f(x)) { cons(x,a) } else { a })
  }

  def append[B>:A](b: Stream[B]): Stream[B] = {
    foldRight(b)((x, a) => cons(x, a))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((x, a) => f(x) append a)
  }

}

object Stream {

  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def unfold[A, S](z: S)(f: S => Option[ (A,S) ]): Stream[A] = {
    f(z) match {
      case Some((x, xs)) => cons(x, unfold(xs)(f))
      case None => empty[A]
    }
  }

  def from(n:Int): Stream[Int] = {
    unfold(n)(x => Some((x,x+1)))
  }

  def constant[A](a: A): Stream[A] = {
    unfold[A,Unit](())(x => Some((a,())))
  }

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    s1.zipAll(s2).flatMap { case (a,a2) => Stream((a.toList ++ a2.toList): _*) }
}
