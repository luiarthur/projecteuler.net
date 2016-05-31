/*
 * By considering the terms in the Fibonacci sequence whose values do not
 * exceed four million, find the sum of the even-valued terms.
 */

def buildFib(curr: Int, prev: Int, B: Int, acc: Int=0): Int = {
  if (prev + curr > B) acc
  else {
    //buildFib(curr + prev, curr, B, if (curr % 2 ==0) acc+curr+prev else acc) 
    buildFib(curr + prev, curr, B, acc+curr+prev) 
  }
}

//buildFib(1,0,4000000) // this is the sum of all fibs < 4000000. Now I need one that just sums the evens.
buildFib(1,0,4) // this is the sum of all fibs < 4000000. Now I need one that just sums the evens.
