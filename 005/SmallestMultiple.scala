/** Smallest Multiple
 *  2520 is the smallest number that can be divided by each of the numbers from
 *  1 to 10 without any remainder.  What is the smallest positive number
 *  that is evenly divisible by all of the numbers from 1 to 20?
 */

/** *foldLeft(1) means the base should be 1. 
 * With scala >=  2.9. Using fold instead of foldLeft / foldRight does not 
 * process data in any particular order. Preferred for List.par, Array.par, etc.
 */

def leastMultiple(n: Int): Int = {
  def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)

  def lcm(a: Int, b: Int, c: Int = 1): Int = {
    val g = gcd(a,b)
    if ( g == 1 ) a*b*c else lcm(a/g, b/g, g)
  }

  (1 to n).reduce(lcm(_,_)) 
}

leastMultiple(20) // 232792560
