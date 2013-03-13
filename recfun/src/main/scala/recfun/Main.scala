package recfun

import common._
import annotation.tailrec

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
  def pascal(c: Int, r: Int): Int =
    if (r == 0)
      if (c == 2)
        1
      else
        0
    else
      pascal(c, r - 1) + pascal(c + 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty)
      return true
    else
      chars.head match {
        case '(' => return balanceIn(chars.tail, 1)
        case ')' => return false
        case _ => return balance(chars.tail)
      }


    def balanceIn(chars: => List[Char], inside: Int): Boolean =
      if (chars.isEmpty)
        return false
      else
        chars.head match {
          case '(' => return balanceIn(chars.tail, inside + 1)
          case ')' => return if (inside - 1 == 0) true && balance(chars.tail) else true && balanceIn(chars.tail, inside - 1)
          case _ => return balanceIn(chars.tail, inside)
        }

    balance(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty)
      return 0
    else
      return changeCombinations(money, coins.sorted)

    def changeCombinations(money: Int, coins: => List[Int]): Int =
      if (money == 0)
        return 1
      else
      if (coins.isEmpty && money > 0)
        0
      else
      if (coins.head > money)
        return 0
      else
        return changeCombinations(money - coins.head, coins) + countChange(money, coins.tail)





    countChange(money, coins)
  }
}
