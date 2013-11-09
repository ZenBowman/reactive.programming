package quickcheck

import junit.framework.TestCase
import org.junit.Assert

class TestQuickCheckHeap extends TestCase {
  def testRemovalAndInsertion() {
    val h = new QuickCheckHeap with BinomialHeap
    val q = h.insertAll(List(1, 2, 3, 4, 5))

    Assert.assertEquals(1, h.findMin(q))
    Assert.assertEquals(5, h.findMin(h.deleteN(q, 4)))
  }

  def testMin() {
    val originalList: List[Int] = List(0, 0, 0, 0, 2147483646)
    val f = originalList.min - 2
    val h = new QuickCheckHeap with BinomialHeap
    val q = h.insertAll(originalList :+ f)
    Assert.assertEquals(f, h.findMin(q))
  }
}
