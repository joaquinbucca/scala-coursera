package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    a == findMin(h)
  }

  property("order") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a > b) b else a)
  }
  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("melding") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)

    findMin(meld(h1, h2)) == (if (a > b) b else a)
  }

  property("melding heaps") = forAll { (h1: H, h2: H) =>
    val min1= findMin(h1)
    val min2= findMin(h2)
    findMin(meld(h1, h2)) == (if (min1 > min2) min2 else min1)
  }

  property("associative meld") = forAll { (h1: H, h2: H, h3: H) =>
    val merged1 = meld(h1, meld(h2, h3))
    val merged2 = meld(meld(h1, h2), h3)
    toList(merged1) == toList(merged2)
  }

  property("order of mins") = forAll { h: H =>
    toList(h).zip(toList(h).drop(1)).forall(p => p._1 <= p._2)
  }
}
