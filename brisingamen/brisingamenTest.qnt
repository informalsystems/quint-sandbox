/// A non-deterministic state machine that we use for testing.
///
/// Igor Konnov, Informal Systems, 2023
module brisingamenTest {
  import brisingamen.* from "./brisingamen"

  var balances: Addr -> int
  var balancesRewards: Addr -> int
  var contractState: State
  var userRewards: Addr -> UserRewardInfo

  /// initialize the state machine with a fixed genesis
  action init = all {
    balances' = ADDR.mapBy(a => 10000),
    balancesRewards' =
      ADDR.mapBy(a => if (a == "owner") 100_000 else 0),
    contractState' = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    },
    userRewards' = Set().mapBy(a => nullUser),
  }

  /// The base scenario of the CTF Challenge #9.
  /// https://github.com/oak-security/cosmwasm-ctf/tree/main/ctf-09#execute-entry-points
  action initBase = {
    pure val state0 = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    }
    pure val userRewards0: Addr -> UserRewardInfo = Set().mapBy(a => nullUser)
    pure val result1 = deposit(userRewards0, state0, "USER", 10_000)
    pure val result2 = increase_reward(result1.users, result1.state, "owner", 10_000)
    all {
      contractState' = result2.state,
      userRewards' = result2.users,
      balances' =
        ADDR.mapBy(a => 10_000_000)
          // from the deposit above
          .setBy("USER", old => old - 10_000)
          .setBy(CONTRACT, old => old + 10_000),
      balancesRewards' =
        ADDR.mapBy(a => if (a == "owner") 100_000 else 0)
    }
  }

  /// The action to increase the reward
  action IncreaseReward(sender: Addr, amount: Uint128): bool = all {
    balancesRewards.get(sender) >= amount,
    // transfer `amount` reward tokens to the contract
    balancesRewards' =
      balancesRewards
        .setBy(sender, old => old - amount)
        .setBy(CONTRACT, old => old + amount),
    balances' = balances,
    // call the contract
    val result = increase_reward(userRewards, contractState, sender, amount)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
    }
  }

  /// The action to deposit
  action Deposit(sender: Addr, amount: Uint128): bool = all {
    balances.get(sender) >= amount,
    // transfer `amount` normal tokens to the contract
    balances' =
      balances
        .setBy(sender, old => old - amount)
        .setBy(CONTRACT, old => old + amount),
    balancesRewards' = balancesRewards,
    // call the contract
    val result = deposit(userRewards, contractState, sender, amount)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
    }
  }

  /// The action to withdraw
  action Withdraw(sender: Addr, amount: Uint128): bool = all {
    // call the contract
    val result = withdraw(userRewards, contractState, sender, amount)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
      balancesRewards' = balancesRewards,
      // transfer normal tokens as prescribed by the contract
      balances' =
        balances
          .setBy(result.bankMsg.from_address, old => old - result.bankMsg.amount)
          .setBy(result.bankMsg.to_address, old => old + result.bankMsg.amount),
    }
  }

  /// The action to claim rewards
  action ClaimRewards(sender: Addr): bool = all {
    // call the contract
    val result = claim_rewards(userRewards, contractState, sender)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
      balances' = balances,
      // transfer reward tokens as prescribed by the contract
      balancesRewards' =
        balancesRewards
          .setBy(result.bankMsg.from_address, old => old - result.bankMsg.amount)
          .setBy(result.bankMsg.to_address, old => old + result.bankMsg.amount),
    }
  }

  /// a step that chooses some inputs non-deterministically 
  action step = any {
    val sender = "owner"
    nondet amount = 0.to(balancesRewards.get(sender)).oneOf()
    IncreaseReward(sender, amount),
    //-
    nondet sender = UNPRIVILEGED.oneOf()
    nondet amount = 0.to(balances.get(sender)).oneOf()
    Deposit(sender, amount),
    //-
    nondet sender = UNPRIVILEGED.oneOf()
    nondet amount = 0.to(balances.get(sender)).oneOf()
    all {
      sender.in(keys(userRewards)),
      Withdraw(sender, amount),
    },
    //-
    nondet sender = UNPRIVILEGED.oneOf()
    all {
      sender.in(keys(userRewards)),
      ClaimRewards(sender),
    },
  }

  // an action that does not change the state
  action unchangedAll = all {
    balances' = balances,
    contractState' = contractState,
    userRewards' = userRewards,
    balancesRewards' = balancesRewards,
  }

  /// The test demonstrates unfair distribution of rewards
  run unfairRewardsTest = {
    initBase
      .then(Deposit("USER1", 10_000))
      .then(Deposit("USER2", 10_000))
      .then(IncreaseReward("owner", 1000))
      .then(Withdraw("USER1", 10_000))
      .then(IncreaseReward("owner", 1000))
      .then(IncreaseReward("owner", 1000))
      .then(Deposit("USER1", 10_000))
      .then(
        // query updates the rewards
        val rewards1 =
          update_rewards(userRewards.get("USER1"), contractState).pending_rewards
        val rewards2 =
          update_rewards(userRewards.get("USER2"), contractState).pending_rewards
        all {
          assert(rewards1 == rewards2),
          unchangedAll,
        })
  }

  /// a state invariant that checks whether all balances are non-negative
  val noNegativeBalancesInv = ADDR.forall(a => balances.get(a) >= 0)

  /// a state invariant that checks whether all rewards are non-negative
  val noNegativeRewardsInv = ADDR.forall(a => balancesRewards.get(a) >= 0)

  /// The test demonstrates fair distribution of rewards
  run fairRewardsTest = {
    initBase
        .then(Deposit("USER1", 10_000))
        .then(Deposit("USER2", 10_000))
        .then(IncreaseReward("owner", 1000))
        .then(Withdraw("USER1", 9_999))
        .then(IncreaseReward("owner", 1000))
        .then(IncreaseReward("owner", 1000))
        .then(Deposit("USER1", 9_999))
        .then(
            // query updates the rewards
            val rewards1 = update_rewards(userRewards.get("USER1"), contractState).pending_rewards
            val rewards2 = update_rewards(userRewards.get("USER2"), contractState).pending_rewards
            all {
              assert(rewards1 < rewards2),
              unchangedAll,
            })
  }
}