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
println("GCD of 21 and 14 is " + gcd.getGCD(21,14))
println("LCM of 21 and 14 is " + gcd.getLCM(21,14))
println("GCD of 24 and 48 is " + gcd.getGCD(21,14))
println("LCM of 24 and 48 is " + gcd.getLCM(24,48))
println("GCD of 21 and 41 is " + gcd.getGCD(21,41))
println("LCM of 21 and 41 is " + gcd.getLCM(41,21))
