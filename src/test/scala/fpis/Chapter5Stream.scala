package fpis

import org.scalatest._

class Chapter5Stream extends FunSpec with Matchers {
  sealed trait Stream[+A]{
      def toList: List[A] = this match {
        case Cons(h,t) =>  h()::t().toList
        case Empty => Nil 
      }
      def take(n:Int):Stream[A] = this match {
        case Cons(h, t) =>  if(n==0) Empty else Cons(h, ()=> t().take(n-1)) 
        case Empty => Empty
      }
      def drop(n:Int):Stream[A] = this match {
        case Cons(h, t) =>  if(n==0) Cons(h,t) else t().drop(n-1)
        case Empty => Empty
      }
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
  def assertEqual[A](a:Stream[A],b:Stream[A]) = assert(a.toList == b.toList)

  describe("Stream") {

    it("5.1") {
     assert(Stream(1,2,3).toList == List(1,2,3))
    }
    it("5.2") {
     assertEqual(Stream(1,2,3).take(2), Stream(1,2))
     assertEqual(Stream(1,2,3,4,5).drop(3), Stream(4,5))
    }

  }

}
