package recfun
import common._
import scala.annotation.tailrec

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
    if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanced(chars: List[Char], opened: Int): Boolean = {
      if (opened < 0) false
      else if (chars.isEmpty) opened == 0
      else if (chars.head == '(') balanced(chars.tail, opened + 1)
      else if (chars.head == ')') balanced(chars.tail, opened - 1)
      else balanced(chars.tail, opened)
    }

    balanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money < 0 ) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
