package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
////  
    test("1lineOfSight should correctly handle an array of size 4") {
      val output = new Array[Float](4)
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 3)
      assert(output.toList == List(0f, 1f, 4f, 4f))
    }
////  //
    test("2lineOfSight should correctly handle an array of size 4") {
      val output = new Array[Float](8)
      //                          0   1   2   3   4     5    6  7 
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 16f, 25f, 36f, 49f), output, 3)
      assert(output.toList == List(0f, 1f, 4f, 4f, 4f, 5f, 6f, 7f))
    }
 
  test("4lineOfSight should correctly handle an array of size 4") {
      val output = new Array[Float](6)
      //                          0   1   2   3   4     5    6  7 8
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 16f, 25f), output, 1)
      assert(output.toList == List(0f, 1f, 4f, 4f, 4f, 5f))
    }
////  
    test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
      val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
      assert(res == 4f)
    }
////    
////       test("par upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
////        val res = upsweep(Array[Float](0f, 1f, 8f, 9f,20f), 0, 5,2)
////        assert(res == 4f)
////      }
////  
    test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
      val output = new Array[Float](4)
      downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
      assert(output.toList == List(0f, 1f, 4f, 4f))
    }
//  
//   // 0.0, 7.0, 7.0, 11.0, 12.0
     test("2downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
      val output = new Array[Float](5)
      val t=upsweep(Array[Float](0f, 7f, 14f, 33f,48f), 1, 5,3)
      println(t)
      downsweep(Array[Float](0f, 7f, 14f, 33f,48f),output, 0f ,t)
    //  downsweepSequential(Array[Float](0f, 7f, 14f, 33f,48f), output, 0f,1,5)
      assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
    }
//
  test("3downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    val t = upsweep(Array[Float](0f, 7f, 14f, 33f, 48f), 1, 5, 1)
    downsweep(Array[Float](0f, 7f, 14f, 33f, 48f), output, 8f, t)
    //  downsweepSequential(Array[Float](0f, 7f, 14f, 33f,48f), output, 0f,1,5)
    assert(output.toList == List(0.0, 8.0, 8.0, 11.0, 12.0))
  }

}

