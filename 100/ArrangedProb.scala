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

class Rational (n: Long, d: Long) {
  require(d != 0)
  def gcd(a: Long, b: Long): Long = if (b==0) a else gcd(b, a%b) // Euclid's Algorithm
  private val g = gcd(n,d)
  val numer = n / g
  val denom = d / g
  override def toString = numer + "/" + denom
  def times(that: Rational): Rational = { 
    val g1 = gcd(this.numer,that.denom)
    val g2 = gcd(that.numer,this.denom)
    new Rational(this.numer / g1 * that .numer / g2, that.denom / g1 * this.denom / g2)
  }
  def * (that: Rational): Rational = times(that)
  def == (that: Rational): Boolean = this.numer == that.numer && this.denom == that.denom
}

/*
val x = new Rational(4,3)
val y = new Rational(3,2)
val z = new Rational(6,4)
y * x
y == x
y == z
new Rational(4,8) == new Rational(1,2)
*/

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
