package fpis

import org.scalatest._

class Chapter4Either extends FunSpec with Matchers {
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]  = this match {
      case Right(r)  => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
      case Right(r)  => f(r)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]  = this match{
      case k:Right[A]  => k
      case _ => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(x => b.map(y => f(x,y)) )
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  describe("Either") {
    it("4.6") {
      assert(Right(3).map(_ * 2) == Right(6))

      assert(Right(3).flatMap(k => Right( k * 2)) == Right(6))

      assert(Right(3).orElse(Right( 2)) == Right(3))
      assert(Left(3).orElse(Right( 2)) == Right(2))

      assert(Right(4).map2(Right(4))(_ *_) == Right(16))
    }

    it("4.7") {
      def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldLeft[Either[E, List[A]]](Right(List[A]()))((c,a)=> {
          c.flatMap(av => a.map(cv => av :+ cv))
        })
      def traverse[E, A, B](as: List[A])( f: A => Either[E, B]): Either[E, List[B]] = as.foldLeft[Either[E, List[B]]](Right(List[B]())) ((a,c)=> {
        a.flatMap(av =>  f(c).map(cv=> av :+ cv) )
      })

      assert(sequence(List(Right(1),Right(2),Right(3))) == Right(List(1,2,3)))
      assert(traverse(List(1,2,3))(Right(_)) == Right(List(1,2,3)))

    }

  }

}