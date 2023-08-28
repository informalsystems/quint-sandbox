/**
 * A Fast Majority Vote Algorithm by J Strother Moore.
 *
 * See: https://www.cs.utexas.edu/users/boyer/ftp/ics-reports/cmp32.pdf
 */
module mjrty {
  const votes: List[str]
  var cand: str
  var i: int
  var k: int

  action init = all {
    cand' = "",
    i' = 0,
    k' = 0,
  }

  action step = all {
    i < length(votes),
    any {
      all {
        k == 0,
        cand' = votes[i],
        k' = 1,
      },
      all {
        cand == votes[i],
        cand' = cand,
        k' = k + 1,
      },
      all {
        cand != votes[i],
        cand' = cand,
        k' = k - 1,
      },
    },
    i' = i + 1,
  }

  val invariant = {
    (i == length(votes))
      implies
    {
      val candidates = indices(votes).map(j => votes[j])
      val winnerCount = indices(votes).filter(j => votes[j] == cand).size()
      candidates.forall(c =>
        val count = indices(votes).filter(j => votes[j] == c).size()
        count <= winnerCount
      )
    }
  }
}

module mjrtyTest {
  import mjrty(votes = ["A", "A", "A", "C", "C", "B", "B", "C", "C", "C", "A", "A", "A"]) as M
  export M.*
}