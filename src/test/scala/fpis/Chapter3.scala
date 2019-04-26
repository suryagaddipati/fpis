package fpis

import org.scalatest._

class Chapter3 extends FunSpec with Matchers {

  describe("List") {

    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]
    object  List{
      def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    it("3.2") {
      def tail[A](l: List[A]): List[A] = l match {
        case (Cons(_:A,t:List[A])) => t
        case Nil => Nil
      }
      assert(tail(List(1,2,3)) == List(2,3))
      assert(tail(Nil) == Nil)
    }

    it("3.3") {
      def setHead[A](l:List[A], h:A): List[A]= l match {
        case (Cons(_:A,t:List[A])) => Cons(h,t)
        case Nil => Nil
      }
      assert(setHead(List(1,2,3),4) == List(4,2,3))
    }

    it("3.4") {
      def drop[A](l: List[A], n: Int): List[A] = n match {
        case 0 => l
        case _ =>l match {
          case (Cons(_:A,t:List[A])) => drop(t,n-1)
          case Nil => Nil
        }
      }

      assert( drop(List(1,2,3,4),2) == List(3,4))
    }

    it("3.5") {
      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case (Cons(h:A,t:List[A])) => if(f(h)) dropWhile(t,f) else l
        case Nil => Nil
      }

      assert(dropWhile(List(2,4,6,5,3,8), (x:Int) => x%2 == 0  ) == List(5,3,8) )
    }
    it("3.6") {
      def init[A](l: List[A]): List[A]= l match {
        case(Cons(_:A,Nil)) => Nil
        case (Cons(h:A,t:List[A])) => Cons(h,init(t))
        case Nil => Nil
      }

      assert(init(List(1,2,3,4)) == List(1,2,3))
    }

    describe("recursion") {

      def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

      //if you have a recursive function that calls itself as its last action, then you can reuse the stack frame of that function.
      def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
        @annotation.tailrec
        def loop(l: List[A], z: B): B = l match {
          case Nil => z
          case Cons(h,t) => loop(t, f(z,h))
        }
        loop(as,z)
      }
      it("3.9"){
        def length[A](as: List[A]): Int = foldRight(as,0)((_,y) => y+1)
        assert(length(List(1,2,3)) == 3)
      }

      it("3.10"){

        assert(foldLeft(List[Int](1,2,3),0) (_+_) == 6 )
      }

      it("3.12"){
       def reverse[A](l:List[A]): List[A] = foldLeft(l,List[A]())((e,r)=> Cons(r,e))
        assert(reverse(List(1,2,3)) == List(3,2,1))
      }

      it("3.13"){

        def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
          case Nil => z
          case Cons(h,t) => f(foldRight(t,z) ((a,b)=>f(b,a)),h)

        }
        def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
          case Nil => z
          case Cons(h,t) => f(h,foldLeft(t,z) ((a,b)=>f(b,a)))

        }
        assert(foldLeftViaFoldRight(List[Int](1,2,3),0) (_+_) == 6 )
        assert(foldRightViaFoldLeft(List[Int](1,2,3),0) (_+_) == 6 )
      }

     it("3.14") {
      def append[A](l: List[A],a: A): List[A] = foldRight(l,List(a)) ((b,a) => Cons(b,a))
      assert(append(List(1,2,3),4) == List(1,2,3,4))
     }

    }

  }
}

