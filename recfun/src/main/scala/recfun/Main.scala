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
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 
    else pascal(c - 1, r - 1) + pascal(c, r - 1) 

  /**
   * Exercise 2
   */
    
  def balance(chars: List[Char]) = {
    def balance(chars: List[Char], counter: Int): Boolean =
      if(chars.isEmpty) counter == 0
      else if(counter < 0) false
      else balance(chars.tail, increment(chars.head, counter))
    
    def increment(first: Char, count: Int): Int = 
      if(first == '(') count + 1
      else if (first == ')') count - 1
      else count
    
    balance(chars, 0)
  }    

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]) = {
    def countChange(money: Int, coins: List[Int], counter: Int): Int =
      if(coins.isEmpty || money < 0) 0
      else if(money == 0) 1
      else countChange(money - coins.head, coins, counter) + countChange(money, coins.tail, counter + 1)  
      
    countChange(money, coins, 0)
  }
}
