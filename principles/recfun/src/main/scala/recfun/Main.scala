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
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], opens: Int) : Boolean = {
      if (chars.isEmpty) opens == 0
      else {
        chars.head match {
          case '(' => f(chars.tail, opens + 1)
          case ')' => if (opens == 0) false else f(chars.tail, opens -1)
          case _ => f(chars.tail, opens)
        }
      }
    }

    f(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int ={
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}
