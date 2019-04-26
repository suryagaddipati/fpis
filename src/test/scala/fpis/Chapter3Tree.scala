package fpis

import org.scalatest._

class Chapter3Tree extends FunSpec with Matchers {

  describe("Tree") {
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    it("3.24"){
      def size[A](t: Tree[A]): Int =  t match {
        case Leaf(_) => 1
        case Branch(l,r) => 1  + size(l) + size(r)
      }

      assert(size(Leaf(1)) == 1)
      assert(size( Branch(Leaf(1), Leaf(1))) == 3)
    }

    it("3.25"){

      def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l,r) => if(maximum(l)> maximum(r))  maximum(l) else maximum(r)
    }

      assert(maximum(Leaf(1)) == 1)
      assert(maximum( Branch(Leaf(55), Branch(Leaf(1), Leaf(67)))) == 67)
    }
  }
}

