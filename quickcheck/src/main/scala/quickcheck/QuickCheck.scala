package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  case class M() extends Bogus1BinomialHeap with IntHeap;
  lazy val genHeap: Gen[H] = {
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(this.empty), const(this.insert(k, this.insert(k+8,this.empty))),
          const(this.insert(7, this.insert(8,this.insert(9,this.insert(11,this.empty))))), genHeap)
    } yield (m)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("2xinserts and findMin should return min") = forAll { (h: H) =>
    if (isEmpty(h)) {
      findMin(insert(2, insert(1, h))) == 1
    } else {
      true;
    }
  }

  property("after insert do the delete and after that heap should be empty") = forAll { (h: H) =>
    if (isEmpty(h)) {
      isEmpty(deleteMin(insert(1, h))) == true
    } else {
      true
    }
  }
  
  property("intensive delete")= forAll { (h: H) =>
     if (isEmpty(h)) {
      def h2 = insert(1, insert(2, insert(3, h)))
      def h3 = insert(7, insert(6, insert(3, h)))
      def h4 = insert(2, insert(77, insert(3, h)))
      if (findMin(deleteMin(meld(h4,meld(h2, h3)))) == 2) {
        true
      } else {
        false
      }
    } else {
      true
    }
  }

  property("is sorted") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      compare(findAll(h))
    } else {
      true
    }
  }

  property("meld should return min") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      val minH = findMin(h)
      def h2 = insert(1, h)
      if (minH < 1) {
        findMin(meld(h, h2)) == minH
      } else {
        findMin(meld(h, h2)) == 1
      }
    } else {
      true
    }
  }

  def findAll(h: H): List[Int] = {
    if (isEmpty(h)) {
      List()
    } else {
      List(findMin(h)) ::: findAll(deleteMin(h))
    }
  }

  def compare(a: List[Int]): Boolean = {
    if (a.size > 1) {
      if (a.head < a.tail.head) {
        compare(a.tail)
      } else {
        false;
      }
    } else {
      true
    }
  }
}
