/// A mutated version of brisingamen.qnt, to figure out the behavior.
/// To see the changes, do: diff brisingamen.qnt brisingamen_twin.qnt
///
/// Igor Konnov, Informal Systems, 2023
module brisingamen_twin {
  import cosmwasm.* from "./cosmwasm"

  // our predefined addresses
  pure val CONTRACT = "contract"
  pure val UNPRIVILEGED = Set("USER", "USER1", "USER2")
  pure val ADDR = Set("owner", CONTRACT).union(UNPRIVILEGED)

  /// The contract state:
  /// LINK brisingamen/rust/ctf-09/src/state.rs#State
  type State = {
    owner: Addr,
    total_staked: Uint128,
    global_index: Decimal,
  }

  /// User reward info:
  /// LINK brisingamen/rust/ctf-09/src/state.rs#UserRewardInfo
  type UserRewardInfo = {
    staked_amount: Uint128,
    user_index: Decimal,
    pending_rewards: Uint128,
  }

  /// the default user reward
  pure val nullUser: UserRewardInfo = {
    staked_amount: 0,
    user_index: 0,
    pending_rewards: 0,
  }

  /// Cosmwasm bank message:
  /// [BankMsg](https://docs.rs/cosmwasm-std/latest/cosmwasm_std/enum.BankMsg.html)
  type BankMsg = {
    from_address: Addr,
    to_address: Addr,
    amount: int,
  }

  /// the default bank message
  pure val nullBankMsg: BankMsg = { from_address: "", to_address: "", amount: 0 }

  /// A result that can be returned by one of the handlers
  type Result = {
    error: str,
    state: State,
    users: Addr -> UserRewardInfo,
    // when bankMsg.amount == 0, this message should be ignored
    bankMsg: BankMsg,
  }

  /// a contract result, provided that there is no error
  pure def Ok(state: State, users: Addr -> UserRewardInfo): Result = {
    /* return */ { error: "", state: state, users: users, bankMsg: nullBankMsg }
  }

  /// a result by the bank module, provided that there is no error
  pure def OkBank(state: State,
      users: Addr -> UserRewardInfo, bankMsg: BankMsg): Result = {
    /* return */ { error: "", state: state, users: users, bankMsg: bankMsg }
  }

  /// an error result
  pure def Err(msg: str): Result = {
    /* return */ {
      error: msg,
      state: { owner: "", total_staked: 0, global_index: 0 },
      users: ADDR.mapBy(a => nullUser),
      bankMsg: nullBankMsg,
    }
  }

  /// Entry point for owner to increase reward
  /// LINK brisingamen/rust/ctf-09/src/contract.rs#increase_reward
  pure def increase_reward(users: Addr -> UserRewardInfo,
      state: State, sender: Addr, amount: Uint128): Result = {
    // NOTE the caller should have transferred `amount` of tokens (that is, must_pay)
    if (sender != state.owner) {
      /* return */ Err("Unauthorized")
    } else if (state.total_staked == 0) {
      /* return */ Err("NoUserStake")
    } else {
      val returnState: State = {
        ...state,
        global_index: state.global_index + from_ratio(amount, state.total_staked)
      }
      /* return */ Ok(returnState, users)
    }
  }

  /// Entry point for users to deposit funds
  /// LINK brisingamen/rust/ctf-09/src/contract.rs#deposit
  pure def deposit(users: Addr -> UserRewardInfo,
      state0: State, sender: Addr, amount: Uint128): Result = {
    // NOTE the caller should have transferred `amount` of tokens (that is, must_pay)
    pure val user0 =
      if (keys(users).contains(sender)) {
        users.get(sender)
      } else {
        { ...nullUser, user_index: state0.global_index, }
      }

    // update the rewards for the user (immutably)
    pure val user1: UserRewardInfo = update_rewards(user0, state0)
    // update the staked amount (immutably)
    pure val user2 = {
      ...user1,
      staked_amount: user1.staked_amount + amount,
    }
    // update the state (immutably)
    pure val state1: State = {
      ...state0,
      total_staked: state0.total_staked + amount,
    }
    /* return */ Ok(state1, users.put(sender, user2))
  }

  /// Entry point for users to withdraw funds
  /// LINK brisingamen/rust/ctf-09/src/contract.rs#withdraw
  pure def withdraw(users: Addr -> UserRewardInfo,
      state0: State, sender: Addr, amount: Uint128): Result = {
    // NOTE: no check that withdraw does not receive any funds
    pure val user0 = users.get(sender)
    if (amount == 0) {
      /* return */ Err("ZeroAmountWithdrawal")
    } else if (user0.staked_amount < amount) {
      /* return */ Err("WithdrawTooMuch")
    } else {
      pure val user1 = update_rewards(user0, state0)
      pure val user2: UserRewardInfo = {
        ...user1,
        staked_amount: user1.staked_amount - amount,
      }
      pure val state1: State = {
        ...state0,
        total_staked: state0.total_staked - amount,
      }
      // generate a message to send to sender
      pure val bankMsg: BankMsg = {
        from_address: CONTRACT,
        to_address: sender,
        amount: amount
      }
      /* return */ OkBank(state1, users.put(sender, user2), bankMsg)
    }
  }

  /// Entry point for user to claim rewards
  /// LINK brisingamen/rust/ctf-09/src/contract.rs#claim_rewards
  pure def claim_rewards(users: Addr -> UserRewardInfo,
      state0: State, sender: Addr): Result = {
    // no check that withdraw does not receive any funds
    pure val user0 = users.get(sender)
    pure val user1 = update_rewards(user0, state0)
    pure val amount = user1.pending_rewards
    if (amount == 0) {
      /* return */ Err("ZeroRewardClaim")
    } else {
      pure val user2: UserRewardInfo = { ...user1, pending_rewards: 0 }
      pure val bankMsg = {
        from_address: CONTRACT,
        to_address: sender,
        amount: amount
      }
      /* return */ OkBank(state0, users.put(sender, user2), bankMsg)
    }
  }

  /// LINK brisingamen/rust/ctf-09/src/contract.rs#update_rewards
  pure def update_rewards(user: UserRewardInfo, state: State): UserRewardInfo = {
    // We do not understand what the condition below is doing.
    // Hence, we comment it out and compare to the original spec.
    //if (user.staked_amount == 0) {
    //  /* return */ user
    //} else {
      // Calculate pending rewards.
      // Note that Decimal is converted to Uint128, hence the division:
      // https://docs.rs/cosmwasm-std/latest/src/cosmwasm_std/math/decimal.rs.html#650
      pure val reward =
        (state.global_index - user.user_index) * user.staked_amount / DECIMAL_FRACTIONAL
      /* return */ {
        ...user,
        pending_rewards: user.pending_rewards + reward,
        user_index: state.global_index,
      }
    //}
  }
}