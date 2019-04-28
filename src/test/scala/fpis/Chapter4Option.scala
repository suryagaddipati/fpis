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
    def orElse[B >: A](ob: => Option[B]): Option[B] = ???
    def filter(f: A => Boolean): Option[A] = ???
  }

  describe("Option") {


    it("4.1") {
      assert((SomeO(4)).map(_ * 2) == SomeO(8))
      assert((SomeO(4)).flatMap(k => SomeO(k * 2)) == SomeO(8))
      assert((SomeO(4)).getOrElse(0) == 4)
    }

  }

}

