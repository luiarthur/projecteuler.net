/* 
 * The prime factors of 13195 are 5, 7, 13 and 29.  
 * What is the largest prime factor of the number 600851475143 ?
 */

import scala.math.sqrt

def largestPrime(n: Long): Long = {
  def isPrime(n: Long): Boolean = (2 to sqrt(n).toInt).forall( n % _ > 0 )
  def isFactor(n: Long, x: Long) = n % x == 0
  def isPrimeFactor(n: Long, x: Long) = isFactor(n,x) && isPrime(x)
  
  def loop(d: Long): Long = {
    if ( isPrimeFactor(n, d) ) d else loop(if (d==n) sqrt(n).toLong else d-1)
  }

  loop(n).toLong
}


// Alternatively:
def simpleLargestPrime(x: Long, divisor: Long = 2): Long = 
  if (x > 1) {
    if (x % divisor == 0) simpleLargestPrime(x / divisor, divisor)
    else simpleLargestPrime(x, divisor + 1)
  } else divisor


// Run:
val x = 600851475143L
largestPrime(x)
simpleLargestPrime(x)
