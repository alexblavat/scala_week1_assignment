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
    if (c == r || c == 0)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty) false
    else{
      def isBrace (c: Char): Boolean = {
        if (c == ')' || c == '(') return true else return false
      }
      val cleanList = chars.filter(isBrace _)
      if (cleanList.head == ')') return false
      else{
        def cleanMatch(parenthesesArray: List[Char]): Boolean = {
          val stack = new scala.collection.mutable.Stack[Char]
          parenthesesArray.foreach(
            ch => {
              if(ch == '(') {
                stack.push(ch)
              }else {
                try {
                  stack.pop()
                } catch {
                  case e: java.util.NoSuchElementException => return false
                }
              }
            }
          )
          if(stack.isEmpty) return true else return false
        }
        cleanMatch(cleanList)
      }

    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def waysToGiveChange(money: Int, coins: List[Int], ways_ct:Int): Int = {
      if(money < 0) ways_ct
      else if(coins.isEmpty){
        if(money == 0) ways_ct + 1 else ways_ct
      }else{
        waysToGiveChange(money, coins.tail, ways_ct) + waysToGiveChange(money - coins.head, coins, ways_ct)
      }
    }
    waysToGiveChange(money, coins, 0)
  }
}
