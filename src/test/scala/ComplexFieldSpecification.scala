import Lab4.{ComplexNumber, Field}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

object ComplexFieldSpecification extends Properties("Field") {

  implicit val arb: Arbitrary[ComplexNumber] = Arbitrary {
    for {
      re <- Gen.long
      im <- Gen.long
    } yield {
      ComplexNumber(re, im)
    }
  }

  val f = Field.ComplexField
  
  property("addAssoc") = forAll {
    (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) => f.add(f.add(a, b), c) == f.add(a, f.add(b, c))
  }
  property("multiplicationAssoc") = forAll {
    (a: ComplexNumber, b: ComplexNumber, c: ComplexNumber) => f.multiplication(f.multiplication(a, b), c) == f.multiplication(a, f.multiplication(b, c))
  }
  property("addInverses") = forAll {
    (a: ComplexNumber) => f.add(f.additiveInverse(a), a) == f.add(a, f.additiveInverse(a))
  }
  property("multiplyInverse") = forAll {
    (a: ComplexNumber) => f.multiplication(a, f.multiplicativeInverse(a)) == f.multiplication(f.multiplicativeInverse(a), a)
  }
  property("addIdentity") = forAll {
    (a: ComplexNumber) => f.add(f.zero, a) == f.add(a, f.zero)
  }
  property("multiplyIdentity") = forAll {
    (a: ComplexNumber) => f.multiplication(f.one, a) == f.multiplication(a, f.one)
  }
  property("inverseMultiply2") = forAll {
    (a: ComplexNumber, b: ComplexNumber) =>
      (f.multiplication(f.multiplicativeInverse(a), b)
        == (f multiplication(b, f.multiplicativeInverse(a))))
  }
}
