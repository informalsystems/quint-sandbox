// -*- mode: Bluespec; -*-
/**
 * A Fast Majority Vote Algorithm by J Strother Moore.
 *
 * See: https://www.cs.utexas.edu/users/boyer/ftp/ics-reports/cmp32.pdf
 */
module mjrty {
  // an upper bound on the list length
  pure val BOUND = 20

  var votes: int -> str
  var cand: str
  var i: int
  var k: int

  action init = all {
    nondet len = 0.to(BOUND).oneOf()
    pure val boundedSet = 0.to(BOUND).filter(j => j < len) // a hack for Apalache
    nondet v = boundedSet.setOfMaps(Set("A", "B", "C")).oneOf()
    votes' = v,
    cand' = "",
    i' = 0,
    k' = 0,
  }

  action step = all {
    keys(votes).contains(i),
    any {
      all {
        k == 0,
        cand' = votes.get(i),
        k' = 1,
      },
      all {
        k != 0,
        cand == votes.get(i),
        cand' = cand,
        k' = k + 1,
      },
      all {
        k != 0,
        cand != votes.get(i),
        cand' = cand,
        k' = k - 1,
      },
    },
    i' = i + 1,
    votes' = votes,
  }

  // the property of the algorithm?
  val invariant = or {
    // empty input
    votes.keys() == Set(),
    // not finished yet
    keys(votes).contains(i),
    // finished and found a winner
    {
      val winnerCount = keys(votes).filter(j => votes.get(j) == cand).size()
      val otherCount = keys(votes).filter(j => votes.get(j) != cand).size()
      winnerCount > otherCount
      /*
      // this is not the right property:
      val candidates = keys(votes).map(j => votes.get(j))
      candidates.forall(c =>
        val count = keys(votes).filter(j => votes.get(j) == c).size()
        count <= winnerCount
      )
      */
    },
    // finished but there is no majority
    {
      val candidates = keys(votes).map(j => votes.get(j))
      candidates.forall(c =>
        val myCount = keys(votes).filter(j => votes.get(j) == c).size()
        val otherCount = keys(votes).filter(j => votes.get(j) != c).size()
        myCount <= otherCount
      )
    },
  }
}