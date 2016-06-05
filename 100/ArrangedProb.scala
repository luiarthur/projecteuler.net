/** Arranged Probability:
 *
 *  If a box contains twenty-one coloured discs, composed of fifteen blue discs
 *  and six red discs, and two discs were taken at random, it can be seen that
 *  the probability of taking two blue discs, P(BB) = (15/21)(14/20) = 1/2.
 *
 *  The next such arrangement, for which there is exactly 50% chance of taking
 *  two blue discs at random, is a box containing eighty-five blue discs and
 *  thirty-five red discs.
 *
 *  By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
 *  discs in total, determine the number of blue discs that the box would
 *  contain.
 **/

def findNumber(n: Long): (Long,Long) = {
  def pass(n: Long, b: Long): Boolean = b.toDouble / n * (b-1).toDouble / (n-1) == .5
  def loop(n: Long, b: Long): (Long,Long) = {
    //def pass(n: Long, b: Long): Boolean = new Rational(b,n) * new Rational (b-1,n-1) == new Rational(1,2)
    if (pass(n,b)) (n,b)
    else if (b/n.toDouble < .5) loop(n+1,n+1)
    else loop(n,b-1)
  }
  loop(n,n)  
}

findNumber(10)
findNumber(100000)
findNumber(10E11.toLong)
