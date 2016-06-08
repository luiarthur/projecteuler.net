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

def longestColatz(n: Int): Int = {
  def next(x: Int): Int = if (x % 2 == 0) x / 2 else 3*x + 1

  def loop(n: Int, seq: List[Int] = List[Int]()): List[Int] = 
    if (n == 1) 1 :: seq else loop(next(n), n :: seq)

  def search(s: List[Int] = (n to 1 by -1).toList, longestSeq: List[Int] = List[Int]()): List[Int] = {
    def innerSearch(n: Int, seq: List[Int] = List[Int]()): List[Int] = 
      if (n == 1 || s contains n) 1 :: seq else (s contains n) innerSearch(next(n), n :: seq)

    if (s.size == 0) longestSeq else {
      val newSeq = innerSearch(s.head)
      val newLongestSeq = if (longestSeq.size < newSeq.size) newSeq else longestSeq
      search(s diff newSeq, newLongestSeq)
    }
  }

  search().last
}

longestColatz(10)
