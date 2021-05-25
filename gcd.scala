object gcd {
  def getGCD(x: Int, y: Int): Int = {
    def loop ( a: Int, b: Int ): Int =
      if ( ( a % b ) == 0 ) b
      else loop(b, a % b)

    if ( x > y ) loop(x, y)
    else loop(y, x)
  }

  def getLCM(x: Int, y: Int): Int = 
    ( x/getGCD(x, y))*y
}
assert ( 7 == gcd.getGCD(21,14) )
assert ( 8 == gcd.getGCD(24,16))
assert ( 9 == gcd.getGCD(27,9))
assert ( 1 == gcd.getGCD(11,21))
assert ( 48 == gcd.getLCM(24,48))
assert ( 861 == gcd.getLCM(41,21))
assert ( 56 == gcd.getLCM(7,8) )
assert ( 6 == gcd.getLCM(6,3) )
