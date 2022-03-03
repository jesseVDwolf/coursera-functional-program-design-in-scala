package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
      const(empty),
      for {
          i <- arbitrary[Int]
          h <- oneOf(const(empty), genHeap)
      } yield insert(i, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) => 
    val aIsBigger = (a > b)
    val h = insert(a, empty)
    findMin(insert(b, h)) == (if (aIsBigger) b else a)
  }

  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val hd = deleteMin(h)
    hd == empty
  }

  /*
  * Given any heap, you should get a sorted sequence
  * of elements when continually finding and deleting
  * minima.
  */
  property("ord1") = forAll { (h: H) =>
    def checkOrd(prevMin: Int, nextMin: H): Boolean =
      if nextMin == empty then true
      else {
        val m = findMin(nextMin)
        if ord.gteq(prevMin, m) then false
        else checkOrd(m, deleteMin(nextMin))
      }
    if h == empty then true
    else checkOrd(findMin(h), deleteMin(h))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2) then isEmpty(meld(h1, h2))
    else {
      val h3 = meld(h1, h2)
      val m = findMin(h3)
      if isEmpty(h1) then (m == findMin(h2))
      else if isEmpty(h2) then (m == findMin(h1))
      else ((m == findMin(h1)) || (m == findMin(h2)))
    }
  }
