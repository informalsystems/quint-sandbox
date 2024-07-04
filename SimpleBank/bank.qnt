module bank {
  import basicSpells.* from "./spells/basicSpells"

  type Investment = { owner: str, amount: int }

  type BankState = {
    balances: str -> int,
    investments: int -> Investment,
    next_id: int
  }

  type BankResult = Ok(BankState) | Err(str)

  var bank_state: BankState
  var error: Option[str]

  pure def deposit(bank_state: BankState, depositor: str, amount: int): BankResult = {
    if (amount <= 0) {
      Err("Amount should be greater than zero")
    } else {
      val new_bank_state = {
        ...bank_state,
        balances: bank_state.balances.setBy(depositor, current => current + amount)
      }
      Ok(new_bank_state)
    }
  }

  pure def withdraw(bank_state: BankState, withdrawer: str, amount: int): BankResult = {
    if (amount <= 0) {
      Err("Amount should be greater than zero")
    } else if (bank_state.balances.get(withdrawer) < amount) {
      Err("Balance is too low")
    } else {
      val new_bank_state = {
        ...bank_state,
        balances: bank_state.balances
          .setBy(withdrawer, current => current - amount)
      }
      Ok(new_bank_state)
    }
  }

  pure def transfer(bank_state: BankState, sender: str, receiver: str, amount: int): BankResult = {
    if (amount <= 0) {
      Err("Amount should be greater than zero")
    } else if (bank_state.balances.get(sender) < amount) {
      Err("Balance is too low")
    } else {
      val new_bank_state = {
        ...bank_state,
        balances: bank_state.balances
          .setBy(sender, current => current - amount)
          .setBy(receiver, current => current + amount)
      }
      Ok(new_bank_state)
    }
  }

  pure def buy_investment(bank_state: BankState, buyer: str, amount: int): BankResult = {
    if (amount <= 0) {
      Err("Amount should be greater than zero")
    } else if (bank_state.balances.get(buyer) < amount) {
      Err("Balance is too low")
    } else {
      val new_bank_state = {
        next_id: bank_state.next_id + 1,
        investments: bank_state.investments.put(bank_state.next_id, { owner: buyer, amount: amount }),
        balances: bank_state.balances
          .setBy(buyer, current => current - amount)
      }
      Ok(new_bank_state)
    }
  }

  pure def sell_investment(bank_state: BankState, seller: str, id: int): BankResult = {
    if (not(bank_state.investments.has(id))) {
      Err("No investment with this id")
    } else {
      pure val investment = bank_state.investments.get(id)
      if (investment.owner != seller) {
        Err("Seller can't sell an investment they don't own")
      } else {
        val new_bank_state = {
          ...bank_state,
          investments: bank_state.investments.mapRemove(id),
          balances: bank_state.balances
            .setBy(seller, current => current + investment.amount)
        }
        Ok(new_bank_state)
      }
    }
  }

  val ACCOUNTS = Set("Alice", "Bob", "Charlie")
  val AMOUNTS = (-50).to(100)

  action assign_result(r: BankResult): bool = match r {
    | Ok(state) => all {
        bank_state' = state,
        error' = None,
      }
    | Err(err) => all {
        bank_state' = bank_state,
        error' = Some(err),
      }
  }

  action deposit_action = {
    nondet amount = AMOUNTS.oneOf()
    nondet depositor = ACCOUNTS.oneOf()
    val r = deposit(bank_state, depositor, amount)
    assign_result(r)
  }

  action withdraw_action = {
    nondet amount = AMOUNTS.oneOf()
    nondet withdrawer = ACCOUNTS.oneOf()
    val r = withdraw(bank_state, withdrawer, amount)
    assign_result(r)
  }

  action transfer_action = {
    nondet amount = AMOUNTS.oneOf()
    nondet sender = ACCOUNTS.oneOf()
    nondet receiver = ACCOUNTS.oneOf()
    val r = transfer(bank_state, sender, receiver, amount)
    assign_result(r)
  }

  action buy_investment_action = {
    nondet amount = AMOUNTS.oneOf()
    nondet buyer = ACCOUNTS.oneOf()
    val r = buy_investment(bank_state, buyer, amount)
    assign_result(r)
  }

  action sell_investment_action = {
    nondet id = 0.to(10).oneOf()
    nondet seller = ACCOUNTS.oneOf()
    val r = sell_investment(bank_state, seller, id)
    assign_result(r)
  }

  action init = all {
    bank_state' = {
      balances: ACCOUNTS.mapBy(acc => 0),
      investments: Map(),
      next_id: 0
    },
    error' = None,
  }

  action step = any {
    deposit_action,
    withdraw_action,
    transfer_action,
    buy_investment_action,
    sell_investment_action,
  }
}
