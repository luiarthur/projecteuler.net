/*
 * A palindromic number reads the same both ways. The largest palindrome made
 * from the product of two 2-digit numbers is 9009 = 91 × 99. 
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

def largestPalindrome(n: Int): Int = {
  import math.pow
  def isPalindrome(x: Int): Boolean = x.toString == x.toString.reverse
  val range = ( pow(10,n-1).toInt to pow(10,n).toInt )
  val prods = for (x <- range; y <- range) yield x*y
  prods.filter(isPalindrome(_)).max
}

largestPalindrome(3) // 906609
