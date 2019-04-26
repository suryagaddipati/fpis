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
  }
}

