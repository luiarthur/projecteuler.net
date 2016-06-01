/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we
 * get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */

def sumOfMult(a: Int, b: Int, n: Int): Int = 
  (1 to n-1).filter( x => x % a == 0 || x % b == 0).sum

sumOfMult(3,5,1000)
