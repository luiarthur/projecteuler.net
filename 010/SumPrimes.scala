/** Sum Primes
 *  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *  Find the sum of all the primes below two million.
 */

def sumPrimes(n: Int): Long = {
  def isPrime(n: Long): Boolean = (2 to math.sqrt(n).toInt).forall( n % _ > 0 )
  Array.tabulate(n){_.toLong}.filter(x=>isPrime(x)).sum - 1
}

sumPrimes(2000000) // 142913828922
