package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    var t3 = Fork(
      Fork(
        Fork(
          Leaf('a', 3), Leaf('b', 2), List('a', 'b'), 5), Leaf('c', 4), List('a', 'b', 'c'), 9), Leaf('d', 9), List('a', 'b', 'c', 'd'), 18)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree(List('d', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'b', 'a', 'a', 'b', 'a', 'c', 'c', 'c', 'c')) === t3)
    }
  }

  test("createCodeTree2") {
    new TestTrees {
      assert(createCodeTree(List('a', 'a', 'b', 'b', 'b')) === Fork(Leaf('b', 3), Leaf('a', 2), List('b', 'a'), 5))
    }
  }

  test("createCodeTree3") {
    new TestTrees {
      assert(createCodeTree(List('a', 'a', 'b', 'b', 'b', 'd', 'd', 'd', 'd')) === Fork(Fork(Leaf('b', 3), Leaf('a', 2), List('b', 'a'), 5), Leaf('d', 4), List('b', 'a', 'd'), 9))
    }
  }

   
    test("createCodeTree5") {
    new TestTrees {
      val str="Tests that were aborted took too long too complete or crashed the JVM. Such crashes can arise due to infinite non-terminitaing loops or recursion (StackOverflowException) or excessive mamory consumption (OutOfMemoryException)"
      val tree=createCodeTree(string2Chars(str))
      println(decode(tree,encode(tree)(string2Chars(str))))
      
      
      
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
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List( Leaf('e', 1), Leaf('t', 2),Leaf('x', 3)))
  }


  test("combine nil") {
    assert(combine(Nil) === Nil)
  }

  test("combine ein") {
    assert(combine(List(Leaf('e', 1))) === List(Leaf('e', 1)))
  }
  
  
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    //println(leaflist)
    //println(combine(leaflist))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of Nil") {
    val leaflist = Nil
    assert(combine(leaflist) === Nil)
  }

  test("combine of Singeton") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
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
