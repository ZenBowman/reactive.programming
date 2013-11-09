package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def insertAll(l :List[Int]) = {
    var finalHeap = empty
    for (i <- l) {
      finalHeap = insert(i, finalHeap)
    }
    finalHeap
  }

  def deleteN(h: H, n:Int) = {
    var finalHeap = h
    for (i <- 0 until n) {
      finalHeap = deleteMin(finalHeap)
    }
    finalHeap
  }

  property("Minimum of an empty heap") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("Removal is ascending") = forAll {
    (a: Int, b:Int, c:Int, d:Int, e:Int, f: Int, g:Int) =>
      val originalList = List(a,b,c,d,e,f,g)
      val heap = insertAll(originalList)

      var cMin = findMin(heap)
      cMin == originalList.min

      var isAscending = true
      for (i <- 0 until originalList.length) {
        val newCMin = findMin(deleteN(heap, i))
        if (newCMin < cMin) {
          isAscending = false
        }
        cMin = newCMin
      }

      isAscending
  }

  property("The minimum of the union of two heaps is the minimum of the minimums of each heap") = forAll {
    (h1: H, h2: H) =>
      val a = findMin(h1)
      val b = findMin(h2)

      findMin(meld(h1, h2)) == List(a, b).min
  }



  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
