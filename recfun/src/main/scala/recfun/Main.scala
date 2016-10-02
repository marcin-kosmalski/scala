package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {


    def balanceBrackets(openBracketsNum: Int, chars: List[Char]): Boolean = {

      if (chars.size > 0 && openBracketsNum >= 0) {
        if (chars.head == '(') {
          balanceBrackets(openBracketsNum + 1, chars.tail);
        } else if (chars.head == ')') {
          balanceBrackets(openBracketsNum - 1, chars.tail);
        } else {
          balanceBrackets(openBracketsNum, chars.tail);
        }
      }else{
         openBracketsNum == 0;
      }
    }
    if (chars.isEmpty) {
      true;
    }else{
      balanceBrackets(0, chars);
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(sum: Int, money: Int, coins: List[Int]): Int = {
      if (sum == money && money != 0) {
        1;
      } else if (sum > money) {
        0;
      } else if (coins.size == 0) {
        0
      } else {
        count(sum + coins.head, money, coins) + count(sum, money, coins.tail);
      }
    }
    count(0, money, coins)
  }
}
