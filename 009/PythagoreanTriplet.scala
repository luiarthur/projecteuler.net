/** Pythagorean Triplet
 *  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * 
 *  a2 + b2 = c2
 *  For example, 32 + 42 = 9 + 16 = 25 = 52.
 * 
 *  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 *  Find the product abc.
 */

def pythTriplet(n: Int): Int = {
  val mx = n / 2 // a+b>c  => 1000=a+b+c>2c => c<500 => a<500 & b<500
  val ls = for (a <- 1 to mx; b <- 1 to mx; c <- 1 to mx; 
    if (a+b+c == n && a*a + b*b == c*c) )
   yield a*b*c
  ls.toSet.head
}

pythTriplet(1000)
