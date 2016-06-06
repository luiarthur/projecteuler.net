/** Pythagorean Triplet
 *  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * 
 *  a2 + b2 = c2
 *  For example, 32 + 42 = 9 + 16 = 25 = 52.
 * 
 *  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 *  Find the product abc.
 */

// a+b>c  => 1000=a+b+c>2c => c<500 => a<500 & b<500
def pythTriplet(n: Int): Int = {
  {for (c <- 1 to n/2; b <- 1 to c; if (b+c+math.sqrt(c*c-b*b)==1000)) 
    yield b*c*math.sqrt(c*c-b*b).toInt}.head
}

pythTriplet(1000) // 31857000
