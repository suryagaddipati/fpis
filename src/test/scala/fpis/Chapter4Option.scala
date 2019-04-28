package fpis

import org.scalatest._

class Chapter4Option extends FunSpec with Matchers {

  case class SomeO[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case SomeO(a) => SomeO(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match{
      case SomeO(a) => a
      case None => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => SomeO(x)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = flatMap(x => if(f(x)) this else None )
  }

  describe("Option") {


    it("4.1") {
      assert(SomeO(4).map(_ * 2) == SomeO(8))

      assert(SomeO(4).flatMap(k => SomeO(k * 2)) == SomeO(8))

      assert(SomeO(4).getOrElse(0) == 4)

      assert(SomeO(4).orElse(SomeO(8)) == SomeO(4))
      assert(None.orElse(SomeO(8)) == SomeO(8))


      assert(SomeO(4).filter(_%2 ==0) == SomeO(4))
      assert(SomeO(5).filter(_%2 ==0) == None)

    }

    it("4.3"){
      def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap( x => b.map(y => f(x,y)) )
      assert(map2(SomeO(1),SomeO(2))(_ + _) == SomeO(3))
      assert(map2(SomeO(1),None)(_ + _) == None)
    }

    it("4.4"){
      def sequence[A](a: List[Option[A]]): Option[List[A]] =
        a.foldLeft[Option[List[A]]](SomeO(List[A]()))((a,x) => {
          if(a == None || x == None) None
          else x.flatMap( k => a.map(j => j :+ k ) )
        })
      assert(sequence(List(SomeO(1),SomeO(2))) == SomeO(List(1,2)))
      assert(sequence(List(SomeO(1),None)) == None)
    }

  }

}

