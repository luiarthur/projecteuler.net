/*
 * By considering the terms in the Fibonacci sequence whose values do not
 * exceed four million, find the sum of the even-valued terms.
 */

def buildFib(curr: Int, prev: Int, B: Int, acc: Int=1): Int = {
  if (prev + curr > B) acc
  else {
    buildFib(curr + prev, curr, B, acc+curr+prev)
  }
}

buildFib(1,0,4000000) // this is the sum of all fibs < 4000000. Now I need one that just sums the evens.
                      
