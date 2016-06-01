/* 
 * The prime factors of 13195 are 5, 7, 13 and 29.  
 * What is the largest prime factor of the number 600851475143 ?
 */

import scala.math.{sqrt,round}

def largestPrime(n: Double): Double = {
  def isPrime(n: Double): Boolean = (2 to sqrt(n).toInt).forall( n % _ > 0 )
  def isFactor(n: Double, x: Double) = n % x == 0
  def isPrimeFactor(n: Double, x: Double) = isFactor(n,x) && isPrime(x)
  
  def loop(d: Double): Double = 
    if ( isPrimeFactor(n, d) ) d else loop(n, if (d==n) sqrt(n) else d-1)

  loop(n)
}

largestPrime(13195)
largestPrime(600851475143.0)
