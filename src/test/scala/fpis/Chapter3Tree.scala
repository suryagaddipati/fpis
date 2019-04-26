package fpis

import org.scalatest._

class Chapter3Tree extends FunSpec with Matchers {

  describe("Tree") {
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    it("3.25"){
      def size[A](t: Tree[A]): Int =  t match {
        case Leaf(_) => 1
        case Branch(l,r) => 1  + size(l) + size(r)
      }

      assert(size(Leaf(1)) == 1)
      assert(size( Branch(Leaf(1), Leaf(1))) == 3)
    }

    it("3.26"){

      def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l,r) => if(maximum(l)> maximum(r))  maximum(l) else maximum(r)
      }

      assert(maximum(Leaf(1)) == 1)
      assert(maximum( Branch(Leaf(55), Branch(Leaf(1), Leaf(67)))) == 67)
    }

    it("3.27"){

      def depth(t: Tree[Int]): Int = t match {
        case Leaf(v) => 1
        case Branch(l,r) => 1 + ( if(depth(l)> depth(r))  depth(l) else depth(r))
      }

      assert(depth(Leaf(1)) == 1)
      assert(depth( Branch(Leaf(55), Branch(Leaf(1), Leaf(67)))) == 3)
    }
    it("3.28"){
      def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
        case Leaf(v) =>Leaf( f(v))
        case Branch(l,r) => Branch(map(l)(f),map(r)(f))
      }

      assert(map( Branch(Leaf(55), Branch(Leaf(1), Leaf(67))))(_+1) ==  Branch(Leaf(56), Branch(Leaf(2), Leaf(68))))
    }
  }

}

