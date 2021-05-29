import org.scalacheck.{Arbitrary, Gen}

import scala.language.implicitConversions

object Lab4 {
  case class ComplexNumber(real: Long, imagine: Long)

  object ComplexNumber {
    implicit val arb: Arbitrary[ComplexNumber] = Arbitrary {
      for {
        re <- Gen.long
        im <- Gen.long
      } yield {
        ComplexNumber(re, im)
      }
    }
  }

  trait Field[A] {
    def multiplication(a: A, b: A): A

    def add(a: A, b: A): A

    def one: A

    def zero: A

    def additiveInverse(a: A): A

    def multiplicativeInverse(a: A): A
  }

  object Field {
    implicit val ComplexField: Field[ComplexNumber] = new Field[ComplexNumber] {
      override def add(a: ComplexNumber, b: ComplexNumber): ComplexNumber =
        new ComplexNumber(a.real + b.real, a.imagine + b.imagine)

      override def multiplication(a: ComplexNumber, b: ComplexNumber): ComplexNumber = {
        new ComplexNumber(a.real * b.real - a.imagine * b.imagine, a.real * b.imagine + a.imagine * b.real)

      }

      override def one: ComplexNumber = ComplexNumber(1, 0)

      override def zero: ComplexNumber = ComplexNumber(0, 0)

      override def additiveInverse(a: ComplexNumber): ComplexNumber = new ComplexNumber(-a.real, -a.imagine)

      override def multiplicativeInverse(a: ComplexNumber): ComplexNumber = new ComplexNumber(a.real / (a.real * a.real + a.imagine * a.imagine),
        a.imagine / (a.real * a.real + a.imagine * a.imagine))

    }
  }

}