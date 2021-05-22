import org.scalacheck.Prop.forAll
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
    implicit val ComplexField = new Field[ComplexNumber] {
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

  def main(args: Array[String]): Unit = {
    val f = Field.ComplexField
    val addAssoc = forAll {
      (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) => f.add(f.add(a, b), c) == f.add(a, f.add(b, c))
    }
    val multiplicationAssoc = forAll {
      (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) => f.multiplication(f.multiplication(a, b), c) == f.multiplication(a, f.multiplication(b, c))
    }
    val addInverses = forAll {
      (a: ComplexNumber) => f.add(f.additiveInverse(a), a) == f.add(a, f.additiveInverse(a))
    }
    val multiplyInverse = forAll {
      (a: ComplexNumber) => f.multiplication(a, f.multiplicativeInverse(a)) == f.multiplication(f.multiplicativeInverse(a), a)
    }
    val addIdentity = forAll {
      (a: ComplexNumber) => f.add(f.zero, a) == f.add(a, f.zero)
    }
    val multiplyIdentity = forAll {
      (a: ComplexNumber) => f.multiplication(f.one, a) == f.multiplication(a, f.one)
    }
    val inverseMultiply2 = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (f.multiplication(f.multiplicativeInverse(a), b)
        == (f multiplication(b, f.multiplicativeInverse(a))))
    }

    addAssoc.check()
    multiplicationAssoc.check()
    addInverses.check()
    multiplyInverse.check()
    addIdentity.check()
    multiplyIdentity.check()
    inverseMultiply2.check()
  }
}