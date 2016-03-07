package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}