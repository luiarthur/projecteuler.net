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

def longestColatzSlow(n: Int): Int = {
  def next(x: Int): Int = if (x % 2 == 0) x / 2 else 3*x + 1

  def loop(n: Int, seq: List[Int] = List[Int]()): List[Int] = 
    if (n == 1) 1 :: seq else loop(next(n), n :: seq)

  def search(elemToSearch: List[Int], longestSeq: List[Int] = List[Int]()): List[Int] = {
    if (elemToSearch.size == 0) longestSeq else {
      print("\r"+elemToSearch.size)
      val newSeq = loop(elemToSearch.head)
      val newLongestSeq = if (longestSeq.size < newSeq.size) newSeq else longestSeq
      search(elemToSearch diff newSeq, newLongestSeq)
    }
  }

  search(util.Random.shuffle((n to 1 by -1).toList)).last
}

longestColatzSlow(10000)

/** Tactic:
 *    If the chain length has been computed, just use that again.
 */

// Incomplete...
// http://codereview.stackexchange.com/questions/26627/increasing-speed-of-project-euler-14-longest-collatz-sequence
def longestColatz(n: Int): Int = {
  val lengthOf = new Array[Int](n+1)
  lengthOf(1) = 1
  def next(x: Int): Int = if (x % 2 == 0) x / 2 else 3*x + 1
  def distFromEnd(x: Int, nxt: Int): Int = {
    if (lengthOf(x) > 0) lengthOf(x)
    else if (lengthOf(nxt) > 0) { lengthOf(x) = lengthOf(nxt) + 1; lengthOf(x)}
    else distFromEnd(x, next(x))
  }
  def search(toSearch: List[Int], searched: Array[Int] = Array[Int](n), longestChain: Int = 1): Int = {
    val newToSearch = toSearch diff searched.keys.toList
    val hd = toSearch.head
    val newLongest if (toSearch(hd) > toSearch(longestChain)) hd else longestChain
  }
  search((n to 1 by -1).toList)
}

// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
// 10    9    8    7   6    5   4   3   2   1
//distFromEnd(1,nxt=next(1))
//distFromEnd(13,nxt=next(13)) // size should be 10
//distFromEnd(1000000,nxt=next(1000000)) // size should be 153
//distFromEnd(333333,nxt=next(333333)) // size should be 153
