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
 *  discs in total, determine the number of blue dsscs that the box would
 *  contain.
 **/

/* Too Long 
def findNumber(n: Long): (BigInt,BigInt) = {
  val sqrtHalf = math.sqrt(.5)
  def pass(n: BigInt, b: BigInt): Boolean = 
    (n * (n-1)) % (b * (b-1)) == 0 && n * (n-1) / b / (b-1) == 2 
  def loop(n: BigInt, b: BigInt): (BigInt,BigInt) = {
    if (pass(n,b)) (n,b)
    else if (b.toDouble/n.toDouble < sqrtHalf) {
      val next: BigInt = ((n.toLong+1)*math.sqrt(.5)+1).toLong
      loop(n+1,next)
    }
    else loop(n,b-1)
  }
  val N: BigInt = (sqrtHalf*n+2).toLong
  loop(n,N)
}
// The key is that b / n tends to sqrt(1/2). But this is too slow...
*/

// Diophantine Equation
// https://www.alpertron.com.ar/QUAD.HTM
def findNumber(N: Long): (Long, Long) = {
  def loop(n: Long, b: Long): (Long,Long) = {
    if (n<N) loop(4*b+3*n-3, 3*b+2*n-2)
    else (n,b)
  }
  loop(4,3)
}

findNumber(1)
findNumber(10)
findNumber(1000000000000L) // (1070379110497, 756872327473)
