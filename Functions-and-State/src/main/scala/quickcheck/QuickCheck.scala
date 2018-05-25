package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for{
    n <- arbitrary[A]
    h <- frequency((1,Gen.const(empty)),(9,genHeap))
  } yield insert(n,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("finding the minimum of the heap with two elements should get the smallest of the two elements back") = forAll { (x1: A,x2: A) =>
    findMin(insert(x1,insert(x2,empty))) == ord.min(x1,x2)
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll { (n: A) =>
    deleteMin(insert(n,empty)) == empty
  }

  property("Recusivly finding and deleting elements in a Heap should return same elements") = forAll {(h: H) =>
    def isSorted(h:H): Boolean = {
      if(isEmpty(h)) true
      else {
        val min = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (min <= findMin(h2) && isSorted(h2))
      }
    }
    isSorted(h)
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll {(h1: H,h2: H) =>
    val x: A = findMin(meld(h1,h2))
    x == ord.min(findMin(h1),findMin(h2))
  }

  property("The minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H,h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val min = ord.min(m1,m2)
    min == findMin(meld(deleteMin(h1),insert(m1,h2)))
  }

  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H,h2: H) =>
    def isEqual(h1: H,h2: H): Boolean = {
      if(isEmpty(h1) && isEmpty(h2)) true
      else findMin(h1) == findMin(h2) && isEqual(deleteMin(h1),deleteMin(h2))
    }
    isEqual(meld(h1,h2),meld(deleteMin(h1),insert(findMin(h1),h2)))
  }


}