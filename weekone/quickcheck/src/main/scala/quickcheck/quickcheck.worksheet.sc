import quickcheck.BinomialHeap
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

/* only generate even numbers */
val genArb = arbitrary[Int] suchThat (_ % 2 == 0)
val propEven = forAll(genArb) { (n: Int) => (n % 2 == 0) }

propEven.check()

lazy val genMap: Gen[Map[Int,Int]] =
  for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

val propMapSize = forAll(genMap) { (m: Map[Int, Int]) =>
    m.size >= 0
}.check()

genMap.sample

object StringSpecification extends Properties("String") {
  import Prop.forAll

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("endsWith") = forAll { (a: String, b: String) =>
    (a+b).endsWith(b)
  }

  property("substring") = forAll { (a: String, b: String) =>
    (a+b).substring(a.length) == b
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }
}

StringSpecification.check()

import quickcheck.*

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
    lazy val genHeap: Gen[H] = oneOf(
        const(empty),
        for {
            i <- arbitrary[Int]
            h <- oneOf(const(empty), genHeap)
        } yield insert(i, h)
    )

def ord = scala.math.Ordering.Int

import scala.util.Sorting

case class Person(name:String, age:Int)
val people = Array(Person("bob", 30), Person("ann", 32), Person("carl", 19))

// sort by age
object AgeOrdering extends Ordering[Person] {
  def compare(a:Person, b:Person) = a.age compare b.age
}
Sorting.quickSort(people)(AgeOrdering)

people.foreach(println)

val qch = new QuickCheckHeap with quickcheck.Bogus4BinomialHeap

qch.genHeap.sample

val seq = Seq[Int](-1, -1679837516)
val ordSeq = Ordering[Int]
seq.sliding(2).forall { case Seq(x, y) => ordSeq.lteq(x, y) }

val a = 2147483647
val b = 2147483647
