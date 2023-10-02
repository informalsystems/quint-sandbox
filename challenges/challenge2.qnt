// -*- mode: Bluespec; -*-
/**
 * A Quint challenge after: 
 * https://solodit.xyz/issues/m-03-error-in-allowance-logic-code4rena-swivel-swivel-v3-contest-git
 *
 * This is a very simplified version of the original protocol.
 * Its purpose is to illustrate how this issue could be understood when using Quint.
 */
module challenge2 {
  // our addresses are simply strings
  type Address = str
  // for the sake of this example, there are just three addresses
  pure val ADDR = Set("Alice", "Bob", "Charlie")

  // our view of the contract's state
  type ContractState = {
    // how many tokens a holder allows a sender to transfer on their behalf
    allowance: (Address, Address) -> int,
    // we keeptrack of redeemed tokens, instead of implementing the actual logic
    redeemed: (Address, Address) -> int,
  }

  // The result returned by a method. If error == "", then the state contains
  // the updated contains the updated state.
  type Result = {
    output: ContractState,
    error: str,
  }

  // produce the result on error/revert
  pure def revert(input: ContractState, error: str): Result =
    { output: input, error: error }
  // produce the result on no error
  pure def ok(output: ContractState): Result =
    { output: output, error: "" }

  // a simplified version of 'withdraw'
  pure def withdraw(input: ContractState, sender: Address,
      amount: int, receiver: Address, holder: Address): Result = {
    if (holder == sender) {
      /* return */ authRedeem(input, sender, receiver, amount)
    } else {
      pure val allowed = input.allowance.get((holder, sender))
      if (allowed >= amount) {
        revert(input, "not approved")
      } else {
        if (allowed - amount < 0) {
          // the underflow check: Solidity does it automatically, Quint does not.
          // allowance[holder][msg.sender] -= amount;
          revert(input, "underflow")
        } else {
          pure val newAllowance =
            input.allowance.set((holder, sender), allowed - amount)
          pure val output = { ...input, allowance: newAllowance }
          /* return */ authRedeem(output, holder, receiver, amount)
        }
      }
    }
  }

  // in this spec, we simply accumulate the redeemed tokens
  pure def authRedeem(input: ContractState, source: Address, destination: Address, amount: int): Result = {
    pure val newRedeemed = input.redeemed.setBy((source, destination), old => old + amount)
    /* return */ ok({ ...input, redeemed: newRedeemed })
  }
  
  // try in repl:
  // val s0 = { allowance: tuples(ADDR, ADDR).mapBy((h, s) => 100), redeemed: tuples(ADDR, ADDR).mapBy((h, s) => 0) }
  // withdraw(s0, "Alice", 20, "Bob", "Charlie")
}