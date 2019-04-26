package fpis

import org.scalatest._

class Chapter4Option extends FunSpec with Matchers {

  describe("Option") {
    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]

    trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case Some(a) => Some(f(a))
        case None => None
      }
      def flatMap[B](f: A => Option[B]): Option[B] = ???
      def getOrElse[B >: A](default: => B): B = ???
      def orElse[B >: A](ob: => Option[B]): Option[B] = ???
      def filter(f: A => Boolean): Option[A] = ???
    }

    it("4.1"){

    }

  }

}

