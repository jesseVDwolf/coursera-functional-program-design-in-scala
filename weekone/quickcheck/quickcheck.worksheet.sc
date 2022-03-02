import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

val l1 = 1 :: 2 :: 3 :: Nil
val l2 = -1 :: l1
val l3 = l1 ::: l2

val propConcatList = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
}

/* should succeed since above statement holds true for all inputs */
propConcatList.check()

val propSqrt = forAll { (n: Int) => scala.math.sqrt(n*n) == n }

/* should fail since the square root can not be taken from negative numbers */
propSqrt.check()
