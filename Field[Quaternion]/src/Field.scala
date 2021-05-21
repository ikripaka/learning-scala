import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import scala.language.implicitConversions

object Lab4 {

  class ComplexNumber(var real: Long, var imagine: Long) {

  }

    trait FieldAlgebra[A] {
      def multiplication(a: A, b: A): Option[A]

      def addition(a: A, b: A): Option[A]

      def division(a: A, b: A): Option[A]

      def inverse(a: A): A
    }

    object FieldAlgebra {
      implicit val ComplexField = new FieldAlgebra[Complex] {
        override def addition(a: ComplexNumber, b: ComplexNumber): ComplexNumber =
          new ComplexNumber(a.real + b.real, a.imagine + b.imagine)

        override def subtraction(a: ComplexNumber, b: ComplexNumber): ComplexNumber =
          new ComplexNumber(a.real - b.real, a.imagine - b.imagine)

        override def multiplication(a: ComplexNumber, b: ComplexNumber): ComplexNumber = {
          if (a.imagine ==0 || a.real == 0 || b.imagine ==0 || b.real == 0){
            //bottom type
          } else{
          new ComplexNumber(a.real * b.real - a.imagine * b.imagine, a.real * b.imagine + a.imagine * b.real)
          }
        }

        override def inverse(a: ComplexNumber): ComplexNumber = new ComplexNumber(-a.real, -a.imagine)
      }
    }

    object Complex {
      implicit val arb = Arbitrary(Gen.frequency(1 -> `0`, 1 -> `1/2`, 1 -> `1`))
    }

    class FieldSyntax[A](a: A)(implicit F: FieldAlgebra[A]) {
      def &&(b: A): A = F.multiplication(a, b)
      def ||(b: A): A = F.addition(a, b)
      def \\(b: A): A = F.subtraction(a, b)
      def !(): A = F.inverse(a)
      //add one more operation "conjugation"
    }

    implicit def hs[A: FieldAlgebra](a: A) = new FieldSyntax(a)
    sealed abstract class Complex(val i: Double) extends Product with Serializable
    case object `0` extends Complex(0)


//  object Field {
//    implicit val realNumbersByModField = new Field[ComplexNumber] {
//      override def multiplication(a: ComplexNumber, b: ComplexNumber): Option[ComplexNumber] = ???
//
//      override def addition(a: ComplexNumber, b: ComplexNumber): Option[ComplexNumber] = a.addition(a,b)
//
//      override def inverse(a: ComplexNumber): ComplexNumber = a.inverse(a)
//
//      override def division(a: ComplexNumber, b: ComplexNumber): Option[ComplexNumber] = multiplication(a, inverse(b))
//    }
//  }

  def main(args: Array[String]): Unit = {
    val assoc1 = forAll{
      (a:ComplexNumber, b:ComplexNumber, c:ComplexNumber) => ((a&&(b&&c)) == ((a&&b)&&c))
    }
    val assoc2 = forAll{
      (a:ComplexNumber, b:ComplexNumber, c:ComplexNumber) => ((a||(b||c)) == ((a||b)||c))
    }
    val equity1 = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (a&&b) == (b&&a)
    }
    val equity2 = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (a||b) == (b||a)
    }
    val addition = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (a||new ComplexNumber(0,0)) ==(new ComplexNumber(0,0)||a
    }

    val multiplication = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (a&&new ComplexNumber(1,1)) ==(new ComplexNumber(1,1)||a
    }

    val singleElement = forAll{
      (a:ComplexNumber, b:ComplexNumber) => (a&&(a!)) ==(a!)&&a
    }



  }
}
