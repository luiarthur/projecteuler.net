/* The sum of the squares of the first ten natural numbers is, 
 *
 * 1^2 + 2^2 + ... + 10^2 = 385 
 *
 * The square of the sum of the first ten natural numbers is, 
 *
 * (1 + 2 + ... + 10)^2 = 55^2 = 3025 
 *
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.  
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */

def ssDiff(n: Int): Int = {
  def sq(x: Int): Int = x*x
  val rng = (1 to n)
  sq(rng.sum) - rng.map( sq(_) ).sum 
}

ssDiff(100) // 25164150
