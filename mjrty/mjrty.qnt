// -*- mode: Bluespec; -*-
/**
 * A Fast Majority Vote Algorithm by J Strother Moore.
 *
 * See: https://www.cs.utexas.edu/users/boyer/ftp/ics-reports/cmp32.pdf
 */
module mjrty {
  // an upper bound on the list length
  pure val BOUND = 10

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
  val postcondition = or {
    // empty input
    votes.keys() == Set(),
    // not finished yet
    keys(votes).contains(i),
    // finished and found a winner
    {
      val winnerCount = keys(votes).filter(j => votes.get(j) == cand).size()
      val otherCount = keys(votes).filter(j => votes.get(j) != cand).size()
      winnerCount > otherCount
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

  // a pure (stateless) inductive invariant for bounded inputs
  pure def pureInv(pVotes: int -> str, pCand: str, pI: int, pK: int): bool = {
    pure val indicesBefore:Set[int] = keys(pVotes).filter(j => (j <= pI))
    pure val potentialCandidates: Set[str] = indicesBefore.map(j => pVotes.get(j))
    pure def noMajority(P: Set[int]): bool = {
      P.forall(c => P.filter(j => pVotes.get(j) == c).size() * 2 < P.size())
    }
    and {
      0 <= pI and pI <= pVotes.keys().size(),
      0 <= pK and pK <= pI,
      powerset(indicesBefore).exists(U =>
        pure val P: Set[int] = indicesBefore.exclude(U)
        and {
          indicesBefore.forall(j => pVotes.get(j) == pCand),
          indicesBefore.size() == pK,
          noMajority(P)
        }
      )
    }
  }

  // we use this action as an initializer
  action indInit = {
    val potentialCandidates: Set[str] = Set("A", "B", "C")
    nondet len = 1.to(BOUND).oneOf()
    pure val boundedSet: Set[int] = 0.to(BOUND).filter(j => j < len) // a hack for Apalache
    nondet ivotes = boundedSet.setOfMaps(potentialCandidates).oneOf()
    nondet icand = potentialCandidates.oneOf()
    nondet ii = 0.to(BOUND).oneOf()
    nondet ik = 0.to(BOUND).oneOf()
    all {
      pureInv(ivotes, icand, ii, ik),
      votes' = ivotes,
      cand' = icand,
      i' = ii,
      k' = ik,
    }
 }

  // This is the inductive invariant we check
  // 1. quint verify --max-steps=1 --main=mjrty --init=indInit --invariant=indInv mjrty.qnt
  // 2. quint verify --max-steps=0 --main=mjrty --init=indInit --invariant=postcondition mjrty.qnt
  val indInv = pureInv(votes, cand, i, k)
}