module bank {
  var balances: str -> int

  pure val ADDRESSES = Set("alice", "bob", "charlie")

  action deposit(account, amount) = {
    balances' = balances.setBy(account, curr => curr + amount)
  }

  action withdraw(account, amount) = all {
    // balances.get(account) >= amount,
    balances' = balances.setBy(account, curr => curr - amount),
  }

  action init = {
    balances' = ADDRESSES.mapBy(_ => 0)
  }

  action step = {
    nondet account = ADDRESSES.oneOf()
    nondet amount = 1.to(100).oneOf()
    any {
      deposit(account, amount),
      withdraw(account, amount),
    }
  }

  val no_negatives = ADDRESSES.forall(addr => balances.get(addr) >= 0)
}
