package recfun
import common._

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
   * Exercise 1 : Pascal's triangle
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2 : Parentheses balancing check
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_inner(chars: List[Char], acc: Int): Boolean =
      if (chars.isEmpty) acc == 0
      else if (acc < 0) false
      else {
        val i = if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0
        balance_inner(chars.tail, acc + i)
      }
    balance_inner(chars, 0)
  }

  /**
   * Exercise 3 : Change generation of an amount
   */

  def countChange(money: Int, coins: List[Int]): Int = 
    if (money == 0)  1
    else if (money < 0 || coins.isEmpty)  0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
