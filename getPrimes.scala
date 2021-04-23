object findPrime {
  def squareRoot(x: Double): Double = {

    def approx(init: Double): Double =
      (init + x/init)/2

    def abs(a: Double ) =
      if ( a < 0 ) -a
      else a

    def loop(guess: Double): Double =
      if ( (abs(guess * guess - x))/x*100 < 0.002 ) guess
      else loop( approx(guess) )

    loop(1)
  }

  def isPrime(n: Int): Boolean = {

    def loop(a: Int, max: Int): Boolean =  {
      if ( a > max ) true
      else if (n % a == 0 ) false
      else loop(a+2, max)
    }

    def startloop(b: Int): Boolean = {
      if ( b % 2 == 0 ) false
      else {
        //val sqroot = math.sqrt(b).toInt
        loop(3, squareRoot(b).toInt)
      }
    }
    startloop(n)
  }

  def getNextPrime(serial: Int): Int =  {
    def loop(i: Int): Int = 
      if ( isPrime(i) )i
      else loop(i + 2 )
    
   if ( serial % 2 == 0 ) loop ( serial + 1 ) 
   else loop(serial)
  }
  def getNPrimes(a: Int): List[Int] = {
    def loop(i: Int, prime: Int, primeList: List[Int]): List[Int] =
      if ( i == a ) primeList
      else {
        val nxtPrime = getNextPrime(prime)
        loop ( i + 1 , getNextPrime(prime+2), nxtPrime::primeList )
      }
    loop(1, 3, List(2))
  }
}

println("17 is a prime : " + findPrime.isPrime(17).toString)
println("15 is a prime : " + findPrime.isPrime(15).toString)
println("Prime after 50  : " + findPrime.getNextPrime(50).toString)
println("First 10 prime numbers  : " + findPrime.getNPrimes(10).toString)
println("500th prime number  : " + findPrime.getNPrimes(10001).head.toString)
