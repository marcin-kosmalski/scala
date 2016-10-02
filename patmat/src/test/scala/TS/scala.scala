package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class scala extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    var t3 = Fork(
      Leaf('d', 9),
      Fork(
        Leaf('c', 4),
        Fork(
          Leaf('a', 3), Leaf('b', 2), List('a', 'b'), 5),
        List('a', 'b', 'c'), 9), List('a', 'b', 'c', 'd'), 18)
  }

  //  abcd18
  //d9     abc9
  //    c4   ab5
  //      a3  b2

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree(List('d', 'd', 'd', 'd','d', 'd', 'd', 'd','d',  'b', 'a', 'a', 'b','a','c','c','c','c')) ===t3)
    }
  }

  test("decode french") {
    new TestTrees {
      assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
    }
  }

  test("times") {
    new TestTrees {

      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }

  test("times 2") {
    new TestTrees {

      assert(times(List('a', 'b', 'a', 'c', 'a', 'a', 'b')) === List(('a', 4), ('b', 2), ('c', 1)))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('x', 3), Leaf('t', 2), Leaf('e', 1)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine nil") {
    assert(combine(Nil) === Nil)
  }

  test("combine ein") {
    assert(combine(List(Leaf('e', 1))) === List(Leaf('e', 1)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("2 decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("ddddabdddd".toList)) === "ddddabdddd".toList)
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t2) == List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("search") {
    new TestTrees {
      assert(codeBits(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))('b') == List(0, 1))
    }
  }

  test("quick encode") {
    new TestTrees {
      assert(encode(t2)("aabbdddaabb".toList) == quickEncode(t2)("aabbdddaabb".toList))
    }
  }

}
