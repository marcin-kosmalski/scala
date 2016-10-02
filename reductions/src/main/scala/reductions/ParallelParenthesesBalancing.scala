package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 10
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceBrackets(openBracketsNum: Int, charsList: List[Char]): Boolean = {
      if (charsList.size > 0 && openBracketsNum >= 0) {
        if (charsList.head == '(') {
          balanceBrackets(openBracketsNum + 1, charsList.tail);
        } else if (charsList.head == ')') {
          balanceBrackets(openBracketsNum - 1, charsList.tail);
        } else {
          balanceBrackets(openBracketsNum, charsList.tail);
        }
      } else {
        openBracketsNum == 0;
      }
    }
    if (chars.size == 0) {
      true;
    } else {
      balanceBrackets(0, chars.toList);
    }
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, pos: Int, neg: Int): Int = {

      if (idx < until) {
        if (chars(idx) == ')') {
          traverse(idx + 1, until, pos, neg + 1)
        } else if (chars(idx) == '(') {
          traverse(idx + 1, until, pos + 1, neg)
        } else {
          traverse(idx + 1, until, pos, neg)
        }
      } else {
        pos - neg
      }
    }

    def reduce(from: Int, until: Int): Int = {
      if ((until - from) <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val result = parallel(reduce(from, mid), reduce(mid, until))
        if (from == 0 && result._1 < 0) {
          Integer.MIN_VALUE
        } else {
          result._1 + result._2
        }
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
