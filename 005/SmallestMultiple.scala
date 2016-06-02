/** Smallest Multiple
 *  2520 is the smallest number that can be divided by each of the numbers from
 *  1 to 10 without any remainder.  What is the smallest positive number
 *  that is evenly divisible by all of the numbers from 1 to 20?
 */

// Why?
def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)

def lcm(a: Int, b: Int, c: Int = 1): Int = {
  val g = gcd(a,b)
  if ( g == 1 ) a*b*c else lcm(a/g, b/g, g)
}

def leastMultiple(n: Int): Int = {
  (1 to n).foldLeft(0)((x,y) => lcm(x,y))
}

leastMultiple(10) // ?

