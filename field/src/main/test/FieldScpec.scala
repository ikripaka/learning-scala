import Lab4.{ComplexNumber, Field}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object FieldSpecification extends Properties("Field") {
  
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
  val inverseMultiply2 = forAll {
    (a: ComplexNumber, b: ComplexNumber) =>
      (f.multiplication(f.multiplicativeInverse(a), b)
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
