// -*- mode: Bluespec; -*-
/// Twin testing of the contract:
/// Compare the original spec and a slightly mutated one.
///
/// Igor Konnov, Informal Systems, 2023
module brisingamenTwinTest {
  import cosmwasm.* from "./cosmwasm"
  import brisingamen as contract from "./brisingamen"
  import brisingamenTwin as twin from "./brisingamenTwin"

  type UserRewardInfo = {
    staked_amount: Uint128,
    user_index: Decimal,
    pending_rewards: Uint128,
  }

  // record a transaction
  type Transaction = {
    kind: str,
    sender: str,
    amount: int,
  }

  // the state of the contract
  var contractBalances: Addr -> int
  var contractBalancesRewards: Addr -> int
  var contractContractState: contract::State
  var contractUserRewards: Addr -> contract::UserRewardInfo
  // the state of the evil twin
  var twinBalances: Addr -> int
  var twinBalancesRewards: Addr -> int
  var twinContractState: twin::State
  var twinUserRewards: Addr -> twin::UserRewardInfo
  // the last executed transaction
  var lastTx: Transaction

  /// The base scenario of the CTF Challenge #9.
  /// https://github.com/oak-security/cosmwasm-ctf/tree/main/ctf-09#execute-entry-points
  action init = {
    pure val state0 = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    }
    pure val userRewards0: Addr -> UserRewardInfo =
      Set().mapBy(a => contract::nullUser)
    pure val contractResult1 =
      contract::deposit(userRewards0, state0, "USER", 10_000)
    pure val contractResult2 =
      contract::increase_reward(contractResult1.users, contractResult1.state, "owner", 10_000)
    pure val twinResult1 =
      twin::deposit(userRewards0, state0, "USER", 10_000)
    pure val twinResult2 =
      twin::increase_reward(twinResult1.users, twinResult1.state, "owner", 10_000)
    all {
      contractContractState' = contractResult2.state,
      contractUserRewards' = contractResult2.users,
      contractBalances' =
        contract::ADDR.mapBy(a => 10_000_000)
          // from the deposit above
          .setBy("USER", old => old - 10_000)
          .setBy(contract::CONTRACT, old => old + 10_000),
      contractBalancesRewards' =
        contract::ADDR.mapBy(a => if (a == "owner") 100_000 else 0),
      // twin copy
      twinContractState' = twinResult2.state,
      twinUserRewards' = twinResult2.users,
      twinBalances' =
        twin::ADDR.mapBy(a => 10_000_000)
          // from the deposit above
          .setBy("USER", old => old - 10_000)
          .setBy(twin::CONTRACT, old => old + 10_000),
      twinBalancesRewards' =
        twin::ADDR.mapBy(a => if (a == "owner") 100_000 else 0),
      lastTx' = { kind: "init", sender: "", amount: 0 }
    }
  }

  // an initial state with a smaller search scope
  action initSmall = {
    pure val state0 = {
      owner: "owner",
      global_index: 0,
      total_staked: 0,
    }
    pure val userRewards0: Addr -> UserRewardInfo =
      Set().mapBy(a => contract::nullUser)
    pure val contractResult1 =
      contract::deposit(userRewards0, state0, "USER", 5)
    pure val contractResult2 =
      contract::increase_reward(contractResult1.users, contractResult1.state, "owner", 5)
    pure val twinResult1 =
      twin::deposit(userRewards0, state0, "USER", 5)
    pure val twinResult2 =
      twin::increase_reward(twinResult1.users, twinResult1.state, "owner", 5)
    all {
      contractContractState' = contractResult2.state,
      contractUserRewards' = contractResult2.users,
      contractBalances' =
        contract::ADDR.mapBy(a => 5)
          // from the deposit above
          .setBy("USER", old => old - 5)
          .setBy(contract::CONTRACT, old => old + 5),
      contractBalancesRewards' =
        contract::ADDR.mapBy(a => if (a == "owner") 5 else 0),
      // twin copy
      twinContractState' = twinResult2.state,
      twinUserRewards' = twinResult2.users,
      twinBalances' =
        twin::ADDR.mapBy(a => 5)
          // from the deposit above
          .setBy("USER", old => old - 5)
          .setBy(twin::CONTRACT, old => old + 5),
      twinBalancesRewards' =
        twin::ADDR.mapBy(a => if (a == "owner") 5 else 0),
      lastTx' = { kind: "init", sender: "", amount: 0 },
    }
  }


  /// The action to deposit
  action Deposit(sender: Addr, amount: Uint128): bool = all {
    contractBalances.get(sender) >= amount,
    twinBalances.get(sender) >= amount,
    // transfer `amount` normal tokens to the contract
    contractBalances' =
      contractBalances
        .setBy(sender, old => old - amount)
        .setBy(contract::CONTRACT, old => old + amount),
    twinBalances' =
      twinBalances
        .setBy(sender, old => old - amount)
        .setBy(twin::CONTRACT, old => old + amount),
    contractBalancesRewards' = contractBalancesRewards,
    twinBalancesRewards' = twinBalancesRewards,
    // call the contract
    val contractResult = contract::deposit(contractUserRewards, contractContractState, sender, amount)
    val twinResult = twin::deposit(twinUserRewards, twinContractState, sender, amount)
    all {
      contractResult.error == "",
      contractContractState' = contractResult.state,
      contractUserRewards' = contractResult.users,
      // twin copy
      twinResult.error == "",
      twinContractState' = twinResult.state,
      twinUserRewards' = twinResult.users,
      lastTx' = { kind: "Deposit", sender: sender, amount: amount },
     }
  }

  /// The action to withdraw
  action Withdraw(sender: Addr, amount: Uint128): bool = all {
    // call the contract
    val contractResult = contract::withdraw(contractUserRewards, contractContractState, sender, amount)
    val twinResult = twin::withdraw(twinUserRewards, twinContractState, sender, amount)
    all {
      contractResult.error == "",
      contractContractState' = contractResult.state,
      contractUserRewards' = contractResult.users,
      contractBalancesRewards' = contractBalancesRewards,
      // transfer normal tokens as prescribed by the contract
      contractBalances' =
        contractBalances
          .setBy(contractResult.bankMsg.from_address, old => old - contractResult.bankMsg.amount)
          .setBy(contractResult.bankMsg.to_address, old => old + contractResult.bankMsg.amount),
      // twin copy
      twinResult.error == "",
      twinContractState' = twinResult.state,
      twinUserRewards' = twinResult.users,
      twinBalancesRewards' = twinBalancesRewards,
      // transfer normal tokens as prescribed by the contract
      twinBalances' =
        twinBalances
          .setBy(twinResult.bankMsg.from_address, old => old - twinResult.bankMsg.amount)
          .setBy(twinResult.bankMsg.to_address, old => old + twinResult.bankMsg.amount),
      lastTx' = { kind: "Withdraw", sender: sender, amount: amount },
     }
  }

  /// The action to increase the reward
  action IncreaseReward(sender: Addr, amount: Uint128): bool = all {
    contractBalancesRewards.get(sender) >= amount,
    // transfer `amount` reward tokens to the contract
    contractBalancesRewards' =
      contractBalancesRewards
        .setBy(sender, old => old - amount)
        .setBy(contract::CONTRACT, old => old + amount),
    contractBalances' = contractBalances,
    // twin copy
    twinBalancesRewards.get(sender) >= amount,
    // transfer `amount` reward tokens to the twin
    twinBalancesRewards' =
      twinBalancesRewards
        .setBy(sender, old => old - amount)
        .setBy(twin::CONTRACT, old => old + amount),
    twinBalances' = twinBalances,
     // call the contract
    val contractResult = contract::increase_reward(contractUserRewards, contractContractState, sender, amount)
    val twinResult = twin::increase_reward(twinUserRewards, twinContractState, sender, amount)
    all {
      contractResult.error == "",
      contractContractState' = contractResult.state,
      contractUserRewards' = contractResult.users,
      // twin copy
      twinResult.error == "",
      twinContractState' = twinResult.state,
      twinUserRewards' = twinResult.users,
      lastTx' = { kind: "IncreaseReward", sender: sender, amount: amount },
    }
  }

  /// The action to claim rewards
  action ClaimRewards(sender: Addr): bool = all {
    // call the contract
    val contractResult = contract::claim_rewards(contractUserRewards, contractContractState, sender)
    val twinResult = twin::claim_rewards(twinUserRewards, twinContractState, sender)
    all {
      contractResult.error == "",
      contractContractState' = contractResult.state,
      contractUserRewards' = contractResult.users,
      contractBalances' = contractBalances,
      // transfer reward tokens as prescribed by the contract
      contractBalancesRewards' =
        contractBalancesRewards
          .setBy(contractResult.bankMsg.from_address, old => old - contractResult.bankMsg.amount)
          .setBy(contractResult.bankMsg.to_address, old => old + contractResult.bankMsg.amount),
      // twin copy
      twinResult.error == "",
      twinContractState' = twinResult.state,
      twinUserRewards' = twinResult.users,
      twinBalances' = twinBalances,
      // transfer reward tokens as prescribed by the contract
      twinBalancesRewards' =
        twinBalancesRewards
          .setBy(twinResult.bankMsg.from_address, old => old - twinResult.bankMsg.amount)
          .setBy(twinResult.bankMsg.to_address, old => old + twinResult.bankMsg.amount),
      lastTx' = { kind: "ClaimRewards", sender: sender, amount: 0 },
     }
  }

  /// a step that chooses some inputs non-deterministically 
  action step = any {
    nondet sender = contract::UNPRIVILEGED.oneOf()
    nondet amount = 0.to(contractBalances.get(sender)).oneOf()
    Deposit(sender, amount),
    // ----
    nondet sender = contract::UNPRIVILEGED.oneOf()
    nondet amount = 0.to(contractBalances.get(sender)).oneOf()
    all {
      sender.in(keys(contractUserRewards)),
      Withdraw(sender, amount),
    },
    // ----
    val sender = "owner"
    nondet amount = 0.to(contractBalancesRewards.get(sender)).oneOf()
    IncreaseReward(sender, amount),
    // ----
    nondet sender = contract::UNPRIVILEGED.oneOf()
    all {
      sender.in(keys(contractUserRewards)),
      ClaimRewards(sender),
    },
  }

  // this invariant lets us to see diverging behavior
  // between the contract and its twin
  val sameBalancesInv = {
    contractBalancesRewards == twinBalancesRewards
  }

  /// a state invariant that checks whether all balances are non-negative
  val noNegativeBalancesInv =
    contract::ADDR.forall(a => contractBalances.get(a) >= 0)

  /// a state invariant that checks whether all rewards are non-negative
  val noNegativeRewardsInv =
    contract::ADDR.forall(a => contractBalancesRewards.get(a) >= 0)
}