/**
 *  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 *  What is the 10001-st prime number?
 */

/* Prime Number Theorem 
 * val upperBound: Long  = ( n * (log(n) + log(log(n))) ).toLong
*/

/*
 * Could improve this by only checking if prime for all the odd
 * numbers above 2.
 */

def nthPrime(n: Int): Int = {
  import math.{log,sqrt}
  def isPrime(n: Int): Boolean = (2 to sqrt(n).toInt).forall( n % _ > 0 )
  def loop(i: Int, acc: Int): Int = 
    if (isPrime(i)) 
      if (acc < n) loop(i+1,acc+1) else i
    else loop(i+1,acc)

  loop(1,0)
}
nthPrime(10001) // 104743
