package fpis

import org.scalatest._

class Chapter10Monoid extends FunSpec with Matchers {
  trait Monoid[A] {
    def op(a1: A, a2: A): A // op(op(x,y), z) == op(x, op(y,z))
    def zero: A //: op(x, zero) == x and op(zero, x) == x
  }
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  describe("Monoid") {
    def monoidTest[A](m: Monoid[A], x: A, y: A, z: A) = {

      val op = m.op _
      val zero = m.zero

      assert(op(op(x, y), z) == op(x, op(y, z)))
      assert(op(x, zero) == x)
      assert(op(zero, x) == x)
    }

    it("10.2") {

      def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
        def zero: Option[A] = None
      }
      monoidTest[Option[Int]](optionMonoid, Some(5), Some(6), Some(7))
    }

    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

      def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
      def zero: A => A = (a: A) => a
    }
    it("10.3") {
      val op = endoMonoid[Int].op _
      val x = (x: Int) => x * 2
      val y = (x: Int) => x * 3
      val z = (x: Int) => x * 4
      assert(op(op(x, y), z)(5) == op(x, op(y, z))(5))
    }
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).foldRight(m.zero)(m.op)
    it("10.5") {
      assert(foldMap(List(1, 2, 3), stringMonoid)(_.toString) == "123")

    }
    it("10.6") {
      def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        foldMap(as, endoMonoid[B])(f.curried)(z)

      assert(foldRightViaFoldMap(List(1, 2, 3))(0)(_ + _) == 6)
    }
    it("10.7") {
      def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
        if(v.length ==1) return f(v(0))
        val(h1,h2) =v.splitAt(v.length/2)
         val h1f = foldMapV(h1,m)(f)
         val h2f = foldMapV(h2,m)(f)
        m.op(h1f,h2f)
      }
      assert(foldMapV(IndexedSeq(1, 2, 3), stringMonoid)(_.toString) == "123")
    }
  }

}
