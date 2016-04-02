package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = Math.min(a, b)
    findMin(h) == min
  }

  property("delete single element1") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("delete single element2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    !isEmpty(deleteMin(h))
  }

  property("order after merge") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    isSorted(findMin(h), deleteMin(h))
  }

  property("min after merge") = forAll { (h1: H, h2: H) =>
    val (min1, min2) = (findMin(h1), findMin(h2))
    val h = meld(h1, h2)
    Math.min(min1, min2) == findMin(h)
  }

  property("values merged") = forAll { (h1: H, h2: H) =>
    (values(h1) ++ values(h2)) == values(meld(h1, h2))
  }

  def values(h: H): Set[Int] = isEmpty(h) match {
    case true => Set()
    case false => values(deleteMin(h)) + findMin(h)
  }

  property("sorting") = forAll { h: H =>
    isSorted(findMin(h), deleteMin(h))
  }

  def isSorted(prev: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      prev <= min && isSorted(min, deleteMin(h))
    }
  }


  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
