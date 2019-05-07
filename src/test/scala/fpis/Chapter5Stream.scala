package fpis

import org.scalatest._

class Chapter5Stream extends FunSpec with Matchers {
  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty      => Nil
    }
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
      case Empty      => Empty
    }
    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) => if (n == 0) Cons(h, t) else t().drop(n - 1)
      case Empty      => Empty
    }
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
      case Empty      => Empty
    }
    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => if (p(h())) t().forAll(p) else false
      case Empty      => true
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _          => z
      }
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)(
        (c, a) => if (p(c)) Cons(() => c, () => a) else Empty
      )

    def headOptionViaFoldRight(): Option[A] =
      foldRight[Option[A]](None)((c, a) => Some(c))
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
  def assertEqual[A](a: Stream[A], b: Stream[A]) = assert(a.toList == b.toList)

  describe("Stream") {

    it("5.1") {
      assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    }
    it("5.2") {
      assertEqual(Stream(1, 2, 3).take(2), Stream(1, 2))
      assertEqual(Stream(1, 2, 3, 4, 5).drop(3), Stream(4, 5))
    }

    it("5.3") {
      assertEqual(
        Stream(2, 4, 6, 9, 10, 12).takeWhile(_ % 2 == 0),
        Stream(2, 4, 6)
      )
    }

    it("5.4") {
      assert(Stream(2, 4, 6).forAll(_ % 2 == 0) == true)
      assert(Stream(2, 4, 6, 7, 8).forAll(_ % 2 == 0) == false)
    }

    it("5.5") {
      assertEqual(
        Stream(2, 4, 6, 9, 10, 12).takeWhileViaFoldRight(_ % 2 == 0),
        Stream(2, 4, 6)
      )
    }

    it("5.6") {
      assert(Stream(1, 3, 4).headOptionViaFoldRight.get == 1)
    }

    it("5.7") {
      def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))
      assertEqual(constant(5).take(4), Stream(5, 5, 5, 5))
    }

    it("5.8") {
      def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))
      assertEqual(from(5).take(4), Stream(5, 6, 7, 8))
    }

    it("5.9") {
      def fibs(one: Int = 0, two: Int = 1): Stream[Int] = {
        val nex = Cons(() => one + two, () => fibs(two, one + two))
        if (one == 0) Cons(() => one, () => Cons(() => two, () => nex)) else nex
      }

      assertEqual(fibs().take(7), Stream(0, 1, 1, 2, 3, 5, 8))
    }
  }
}
