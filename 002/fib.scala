/*
 * By considering the terms in the Fibonacci sequence whose values do not
 * exceed four million, find the sum of the even-valued terms.
 */

def sumEvenFib(curr: Int, prev: Int, B: Int, acc: Int=0): Int = {
  if (curr > B) acc
  else sumEvenFib(curr + prev, curr, B, if (curr % 2 == 0) acc + curr else acc) 
}

sumEvenFib(1,0,4000000) // = 4613732
