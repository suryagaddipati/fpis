package fpis

import org.scalatest._

class Chapter2 extends FunSpec with Matchers {
  describe("fib") {
    def fib(n: Int): Int = if(n == 1 || n==2) 1 else fib(n-1) + fib(n-2)
    it("2.1") {
      assert(fib(3) == 2)
      assert(fib(6) == 8)
    }
  }

  describe("sorted") {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = if(as.size <=1) true
     else ordered(as(0),as(1)) && isSorted(as.drop(1),ordered)
    it("2.2") {
      assert(isSorted(Array(1,2,3),((a:Int,b:Int) => a < b)) == true)
      assert(isSorted(Array(1,2,4,3),((a:Int,b:Int) => a < b)) == false)
    }
  }

}
