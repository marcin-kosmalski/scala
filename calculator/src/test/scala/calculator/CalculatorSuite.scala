package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /**
   * ****************
   * * TWEET LENGTH **
   * ****************
   */

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("calc standard ops") {

    assert(Calculator.calcVal(Plus(Literal(3d), Literal(4d)), Set(), Map[String, Signal[Expr]]()) == 7d)
    assert(Calculator.calcVal(Minus(Literal(10d), Literal(4d)), Set(), Map()) == 6d)
    assert(Calculator.calcVal(Times(Literal(3d), Literal(3d)), Set(), Map()) == 9d)
    assert(Calculator.calcVal(Divide(Literal(6d), Literal(3d)), Set(), Map()) == 2d)
    assert(Calculator.calcVal(Literal(3d), Set(), Map()) == 3d)
    assert(Calculator.calcVal(Plus(Minus(Literal(5d), Literal(4d)), Literal(1)), Set(), Map()) == 2d)
  }

  test("calc with refs ops") {
    def map = Map[String, Signal[Expr]](("a" -> Var(Plus(Literal(3d), Literal(4d)))), ("b" -> Var(Plus(Ref("a"), Literal(1d)))))
    assert(Calculator.calcVal(Plus(Minus(Ref("b"), Literal(5d)), Literal(1)), Set(), map) == 4d)
  }

  test("calc with refs conflict") {
    def mapMitConflict = Map[String, Signal[Expr]](("a" -> Var(Plus(Ref("b"), Literal(4d)))),
      ("b" -> Var(Plus(Ref("c"), Literal(1d)))),
      ("c" -> Var(Plus(Ref("a"), Literal(1d)))))
    assert(Calculator.calcVal(Plus(Minus(Ref("a"), Literal(6d)), Literal(1)), Set(), mapMitConflict).toString() == "NaN")
  }
  
  test("polynomial"){
    assert(Polynomial.computeDelta(Var(2),Var(3) , Var(5))()== -31)
    assert(Polynomial.computeDelta(Var(0),Var(0) , Var(0))()== 0)
    assert(Polynomial.computeDelta(Var(2),Var(3) , Var(5))()== -31)
    assert(Polynomial.computeSolutions(Var(2),Var(6),Var(4),Polynomial.computeDelta(Var(2),Var(6),Var(4)))()== Set(-1.0,-2.0))
    //2x^2+6x+4
  }
  
    test("calc with unknown refs ") {
    def mapMitConflict = Map[String, Signal[Expr]](("a" -> Var(Plus(Ref("b"), Literal(4d)))),
      ("b" -> Var(Plus(Ref("c"), Literal(1d)))),
      ("c" -> Var(Plus(Ref("g"), Literal(1d)))))
    assert(Calculator.calcVal(Plus(Minus(Ref("a"), Literal(6d)), Literal(1)), Set(), mapMitConflict).toString() == "NaN")
  }

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

}
