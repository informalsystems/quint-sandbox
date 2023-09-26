/// A specification of the Challenge 09: Brisingamen.
///
/// https://github.com/oak-security/cosmwasm-ctf/tree/main/ctf-09
///
/// Igor Konnov, Informal Systems, 2023
module brisingamen {
  /// addresses
  type Addr = str
  /// 128-bit integers
  type Uint128 = int
  /// we simply model decimals as big integers
  type Decimal = int

  /// The divisor used in
  /// [Decimal](https://docs.rs/cosmwasm-std/latest/cosmwasm_std/struct.Decimal.html)
  /// to obtain a fixed-point decimal
  pure val DECIMAL_FRACTIONAL = 1_000000_000000_000000

  /// Construct a decimal from a nominator and denominator.
  /// [Decimal::from_ratio](https://docs.rs/cosmwasm-std/latest/cosmwasm_std/struct.Decimal.html#method.from_ratio)
  pure def from_ratio(nominator: int, denominator: int): Decimal = {
    nominator * DECIMAL_FRACTIONAL / denominator
  }

  // our predefined addresses
  pure val CONTRACT = "contract"
  pure val UNPRIVILIGED = Set("USER", "USER1", "USER2")
  pure val ADDR = Set("owner", CONTRACT).union(UNPRIVILIGED)

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
    if (sender != state.owner) {
      Err("Unauthorized")
    } else if (state.total_staked == 0) {
      Err("NoUserStake")
    } else {
      val returnState: State = {
        ...state,
        global_index: state.global_index + from_ratio(amount, state.total_staked)
      }
      Ok(returnState, users)
    }
  }

  /// Entry point for users to deposit funds
  /// LINK brisingamen/rust/ctf-09/src/contract.rs#deposit
  pure def deposit(users: Addr -> UserRewardInfo,
      state0: State, sender: Addr, amount: Uint128): Result = {
    pure val user0 =
      if (keys(users).contains(sender)) {
        users.get(sender)
      } else {
        { ...nullUser, user_index: state0.global_index, }
      }

    // update the rewards for the user (immutably)
    pure val user1 = update_rewards(user0, state0)
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
      Err("ZeroAmountWithdrawal")
    } else if (user0.staked_amount < amount) {
      Err("WithdrawTooMuch")
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
        from_address: "contract",
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
      Err("ZeroRewardClaim")
    } else {
      pure val user2: UserRewardInfo = { ...user1, pending_rewards: 0 }
      pure val bankMsg = {
        from_address: "contract",
        to_address: sender,
        amount: amount
      }
      /* return */ OkBank(state0, users.put(sender, user2), bankMsg)
    }
  }

  /// LINK brisingamen/rust/ctf-09/src/contract.rs#update_rewards
  pure def update_rewards(user: UserRewardInfo, state: State): UserRewardInfo = {
    if (user.staked_amount == 0) {
      user
    } else {
      // calculate pending rewards (note that Dec is converted to Uint128)
      pure val reward =
        (state.global_index - user.user_index) * user.staked_amount / DECIMAL_FRACTIONAL
      {
        ...user,
        pending_rewards: user.pending_rewards + reward,
        user_index: state.global_index,
      }
    }
  }
}