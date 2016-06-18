/** Longest Colatz Sequence
 *  The following iterative sequence is defined for the set of positive
 *  integers:
 *
 *  n → n/2 (n is even) n → 3n + 1 (n is odd)
 *
 *  Using the rule above and starting with 13, we generate the following
 *  sequence:
 *
 *  13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1 It can be seen that this
 *     sequence (starting at 13 and finishing at 1) contains 10 terms. Although
 *     it has not been proved yet (Collatz Problem), it is thought that all
 *     starting numbers finish at 1.
 *
 *  Which starting number, under one million, produces the longest chain?
 *
 *  NOTE: Once the chain starts the terms are allowed to go above one million.
 */

// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
// 10    9    8    7   6    5   4   3   2   1
def longestColatz(n: Int): Int = {
  def next(x: Long): Long = if (x % 2 == 0) x / 2 else 3*x + 1
  def collatz(x: Long, acc: Int = 1): Int = if (x == 1) acc else collatz(next(x),acc+1)
  val out = (1 to n).par.map(collatz(_)) zip (1 to n)
  out.maxBy(_._1)._2
}

longestColatz(1000000) // 837799
