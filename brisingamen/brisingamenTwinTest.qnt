// -*- mode: Bluespec; -*-
/// Twin testing of the contract:
/// Compare the original spec and a slightly mutated one.
///
/// Igor Konnov, Informal Systems, 2023
module brisingamenTwinTest {
  import brisingamen as original from "./brisingamen"
  import brisingamen as twin from "./brisingamen_twin"

  var balances: original::Addr -> int
  var balancesRewards: original::Addr -> int
  var contractState: original::State
  var userRewards: original::Addr -> original::UserRewardInfo

  /// initialize the state machine with a fixed genesis
  action init = all {
    balances' = original::ADDR.mapBy(a => 10000),
    balancesRewards' =
      original::ADDR.mapBy(a => if (a == "owner") 100_000 else 0),
    contractState' = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    },
    userRewards' = Set().mapBy(a => original::nullUser),
  }

  /// The base scenario of the CTF Challenge #9.
  /// https://github.com/oak-security/cosmwasm-ctf/tree/main/ctf-09#execute-entry-points
  action initBase = {
    pure val state0 = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    }
    pure val userRewards0: original::Addr -> original::UserRewardInfo =
      Set().mapBy(a => original::nullUser)
    pure val result1 = original::deposit(userRewards0, state0, "USER", 10_000)
    pure val result2 = original::increase_reward(result1.users, result1.state, "owner", 10_000)
    all {
      contractState' = result2.state,
      userRewards' = result2.users,
      balances' =
        original::ADDR.mapBy(a => 10_000_000)
          // from the deposit above
          .setBy("USER", old => old - 10_000)
          .setBy(original::CONTRACT, old => old + 10_000),
      balancesRewards' =
        original::ADDR.mapBy(a => if (a == "owner") 100_000 else 0)
    }
  }

  /// The action to deposit
  action Deposit(sender: original::Addr, amount: original::Uint128): bool = all {
    balances.get(sender) >= amount,
    // transfer `amount` normal tokens to the contract
    balances' =
      balances
        .setBy(sender, old => old - amount)
        .setBy(original::CONTRACT, old => old + amount),
    balancesRewards' = balancesRewards,
    // call the contract
    val result = original::deposit(userRewards, contractState, sender, amount)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
    }
  }

  /// The action to withdraw
  action Withdraw(sender: original::Addr, amount: original::Uint128): bool = all {
    // call the contract
    val result = original::withdraw(userRewards, contractState, sender, amount)
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

  /// The action to increase the reward
  action IncreaseReward(sender: original::Addr, amount: original::Uint128): bool = all {
    balancesRewards.get(sender) >= amount,
    // transfer `amount` reward tokens to the contract
    balancesRewards' =
      balancesRewards
        .setBy(sender, old => old - amount)
        .setBy(original::CONTRACT, old => old + amount),
    balances' = balances,
    // call the contract
    val result = original::increase_reward(userRewards, contractState, sender, amount)
    all {
      result.error == "",
      contractState' = result.state,
      userRewards' = result.users,
    }
  }

  /// The action to claim rewards
  action ClaimRewards(sender: original::Addr): bool = all {
    // call the contract
    val result = original::claim_rewards(userRewards, contractState, sender)
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
    nondet sender = original::UNPRIVILEGED.oneOf()
    nondet amount = 0.to(balances.get(sender)).oneOf()
    Deposit(sender, amount),
    // ----
    nondet sender = original::UNPRIVILEGED.oneOf()
    nondet amount = 0.to(balances.get(sender)).oneOf()
    all {
      sender.in(keys(userRewards)),
      Withdraw(sender, amount),
    },
    // ----
    val sender = "owner"
    nondet amount = 0.to(balancesRewards.get(sender)).oneOf()
    IncreaseReward(sender, amount),
    // ----
    nondet sender = original::UNPRIVILEGED.oneOf()
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
}