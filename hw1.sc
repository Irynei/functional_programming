

// task 1
def pascal(c: Int, r: Int): Int = {
  if (c == 0 || r == c) 1
  else if (c < 0 || r < 0 || c > r) -1
  else pascal(c - 1, r - 1) + pascal(c, r - 1)
}
// task1 examples
pascal(0, 2)
pascal(1, 2)
pascal(1, 3)
pascal(3, 5)

// task2
def balance(chars: List[Char]): Boolean = {

  def loop(charList: List[Char], countOpen: Int): Boolean = {
    if(charList.isEmpty) countOpen == 0
    else if(countOpen < 0) false
    else {
      if(charList.head == '(')
        loop(charList.tail, countOpen + 1)
      else if(charList.head == ')')
        loop(charList.tail, countOpen - 1)
      else loop(charList.tail, countOpen)
    }
  }
  loop(chars, 0)
}

//task2 examples
balance("ось ще однин ( тут ( та й тут ) i може) (тут) є збалансованими".toList)
balance("())(".toList)
balance("sd() (())))9(() )(()( 0(0()()()) ) ()(0) 90( )90 (0() 0( ))() )()FR))E()C0d0(C)d0v0d()v )()()c0()d( f ) fsdf".toList)

// task3
def countChange(money: Int, coins: List[Int]): Int = {
  if (coins.isEmpty || money < 0) 0
  else if (money == 0) 1
  else {
    countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}

// task3 examples
countChange(4, List(1, 2))
countChange(10, List(1, 4, 5))
countChange(1000, List(1, 4, 5))