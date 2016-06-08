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

  //def sizer(x: Int, mapSizes: Map[Int,Int]): Map[Int,Int] = {
  //  if (x == 1) Map(1,1) else {
  //    val newMapSizes = mapSizes + (x, loop(x).size) 
  //    sizer(x+1, newMapSizes)
  //  }
  //}

  /*
     val m = Map("a" -> 1)
     val mm = m + ("b" -> 2)
   */

  def search(elemToSearch: List[Int] = (n to 1 by -1).toList, longestSeq: List[Int] = List[Int]()): List[Int] = {
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

//def sizer(x: Int, s: Int = 1): Int = if (x == 1) s else sizer(next(x), s+1)
def longestColatz(n: Int): Int = {
  def next(x: Int): Int = if (x % 2 == 0) x / 2 else 3*x + 1

  // 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  def loop(x: Int, lengthMap: Map[Int,Int] = Map(1 -> 1), s: Int = 1): Map[Int,Int] = {

    var lengthOf = Map(1 -> 1)
    def sizer(x: Int, s: Int = 0): Int = 
      if (lengthOf contains x) lengthOf(x) else {
        val nextX = next(x)
        val sizeNextX = sizer(nextX, s+1)
        lengthOf += (nextX -> (s+sizeNextX))
        sizeNextX
      }

    if (x == 1) lengthMap
    else {
      val nextX = next(x)
      if (lengthMap contains nextX) lengthMap + (x -> lengthMap(nextX) + 1)
      else loop(nextX, lengthMap + if (), 1)
    }
  }
    


  def mapper(x: Int)


  /*
     val m = Map("a" -> 1)
     val mm = m + ("b" -> 2)
   */
}

