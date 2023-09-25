// -*- mode: Bluespec; -*-
/* ****************************************************************************
  This encodes the a very simplified proof-of-stake system.

  Manuel Bravo, Informal Systems, 2023
**************************************************************************** */

module buggyCosmoverse {

    /* ****************************************************************************
    * Data Types
    * ************************************************************************* */

    // Represent addresses as strings
    type Address = str

    // Delegator state
    type DelegatorState = {
      address: Address,
      balance: int
    }

    // Validator state
    type ValidatorState = {
      address: Address,
      bonded: Address -> int,
      unbonded: int,
      stake: int
    }

    // Proof-of-stake system state
    type PosState = {
      posAccount: int
    }

    // Result record returned by any of the three PoS functions: delegate, unbond and withdraw
    type Result = {
      success: bool,
      delegatorState: DelegatorState,
      validatorState: ValidatorState,
      posState: PosState
    }

    /**************************************************************************
    * Functions
    * ************************************************************************* */

    /*
    * The function delegate is called when a user wants to delegate tokens to a validator.
    * 1. First it checks that the user has enough tokens in its account.
    * 2. Then it locks those tokens by transferring then from the user's account to the PoS special account.
    * 3. It records that the user has delegated amount tokens to the validator.
    * 4. It increases the validator's stake.
    */
    pure def delegate(delegatorState: DelegatorState, validatorState: ValidatorState, posState: PosState, amount: int): Result = {
      if (amount <= delegatorState.balance) {
        val updatedDelegatorState = delegatorState.with("balance", delegatorState.balance - amount)
        val updatedValidatorState = validatorState.with("stake", validatorState.stake + amount)
                                                  .with("bonded", validatorState.bonded.set(
                                                    delegatorState.address, 
                                                    validatorState.bonded.get(delegatorState.address) + amount
                                                    ))
        val updatedPosState = posState.with("posAccount", posState.posAccount + amount)
        {success: true, delegatorState: updatedDelegatorState, validatorState: updatedValidatorState, posState: updatedPosState}
      } else {
        {success: false, delegatorState: delegatorState, validatorState: validatorState, posState: posState}
      }
    }

    /*
    * The function unbond is called when a user wants to unbond tokens from a validator.
    * 1. First it checks that the user has enough tokens bonded to the validator.
    * 2. Then it records that amount of tokens is unbonded by subtracting amount from bonded and adding to unbonded.
    * 3. Finally, tt decreases the validator's stake.
    */
    pure def unbond(delegatorState: DelegatorState, validatorState: ValidatorState, posState: PosState, amount: int): Result = {
      if (amount < validatorState.bonded.get(delegatorState.address)) {
        val updatedValidatorState = validatorState.with("stake", validatorState.stake - amount)
                                                  .with("bonded", validatorState.bonded.set(
                                                    delegatorState.address, 
                                                    validatorState.bonded.get(delegatorState.address) - amount
                                                    ))
                                                  .with("unbonded", validatorState.unbonded + amount)
        {success: true, delegatorState: delegatorState, validatorState: updatedValidatorState, posState: posState}
      } else {
        {success: false, delegatorState: delegatorState, validatorState: validatorState, posState: posState}
      }
    }

    /*
    * The function withdraw is called when a user wants to withdraw tokens from a validator.
    * 1. First it computes the amount of tokens that can be withdrawn and returns an error in case there are none.
    * 2. Then records that those tokens are withdrawn by setting unbonded to 0.
    * 3. Finally, it unlocks the tokens by transferring them from the PoS special account to the user's account.
    */
    pure def withdraw(delegatorState: DelegatorState, validatorState: ValidatorState, posState: PosState): Result = {
      val amount = validatorState.unbonded
      if (amount > 0) {
        val updatedDelegatorState = delegatorState.with("balance", delegatorState.balance + amount)
        val updatedValidatorState = validatorState.with("unbonded", validatorState.unbonded - amount)
        val updatedPosState = posState.with("posAccount", posState.posAccount - amount)
        {success: true, delegatorState: updatedDelegatorState, validatorState: updatedValidatorState, posState: updatedPosState}
      } else {
        {success: false, delegatorState: delegatorState, validatorState: validatorState, posState: posState}
      }
    }




    /* ****************************************************************************
    * State machine state
    * ************************************************************************* */

    // Delegator state
    var delegators: Address -> DelegatorState
    // Validator state
    var validators: Address -> ValidatorState
    // Proof-of-stake state
    var pos: PosState

    /* ****************************************************************************
    * Execution state
    * ************************************************************************* */

    // Last transaction executed by step.
    // Amount is only relevant for delegate and unbond; it is alwasy 0 for withdraw.
    var lastTx: {tag: str, result: bool, user: Address, validator: Address, amount: int}



    /* ****************************************************************************
    * Actions
    * ************************************************************************* */

    // Future syntactic sugar: helps for better readability. 
    def require(pred:bool):bool = pred

    action actionDelegate(user: Address, validator: Address, amount: int): bool = all {
      val result = delegate(delegators.get(user), validators.get(validator),  pos, amount)
      all {
        delegators' = delegators.set(user, result.delegatorState),
        validators' = validators.set(validator, result.validatorState),
        pos' = result.posState,
        lastTx' = {tag: "Delegate", result: result.success, user: user, validator: validator, amount: amount}
      }
    }

    action actionUnbond(user: Address, validator: Address, amount: int): bool = all {
      val result = unbond(delegators.get(user), validators.get(validator),  pos, amount)
      all {
        delegators' = delegators.set(user, result.delegatorState),
        validators' = validators.set(validator, result.validatorState),
        pos' = result.posState,
        lastTx' = {tag: "Unbond", result: result.success, user: user, validator: validator, amount: amount}
      }
    }

    action actionWithdraw(user: Address, validator: Address): bool = all {
      val result = withdraw(delegators.get(user), validators.get(validator), pos)
      all {
        delegators' = delegators.set(user, result.delegatorState),
        validators' = validators.set(validator, result.validatorState),
        pos' = result.posState,
        lastTx' = {tag: "Withdraw", result: result.success, user: user, validator: validator, amount: 0}
      }
    }

    /* ****************************************************************************
    * Specification Parameters
    * ************************************************************************* */

    // Max uint
    pure val MAX_UINT = 20

    // Users initial balances
    pure val INIT_BALANCE = 20

    // set of all user addresses
    pure val USERS = Set("alice") //, "bob") //, "charlie", "eve")

    // set of all validator addresses
    pure val VALIDATORS = Set("alice", "bob")

    /* ****************************************************************************
    * Invariants
    * ************************************************************************* */

    // Invariant 1: The total amount of tokens is constant.
    val totalAmountTokensConstant = 
      size(USERS)*INIT_BALANCE == 
      USERS.fold(0, (sum, user) => sum + delegators.get(user).balance) + pos.posAccount 

    // Invariant 2: A delegator's balance cannot become negative.
    val balanceGreaterZero = USERS.forall(user => delegators.get(user).balance >= 0)

    // Invariant 3: A validator's stake cannot become negative.
    val stakeGreaterZero = VALIDATORS.forall(validator => validators.get(validator).stake >= 0)

    // Invariant 4: Users cannot steal tokens.
    val balanceLessEqualInitial = USERS.forall(user => delegators.get(user).balance <= INIT_BALANCE)

    // All invariants
    val allInvariants = totalAmountTokensConstant and
                        balanceGreaterZero and
                        stakeGreaterZero and
                        balanceLessEqualInitial
    
    /* ****************************************************************************
    * Execution
    * ************************************************************************* */

    // State initialization: assumes that users start with some initial balance.
    action init: bool = all {
      delegators' = USERS.mapBy(user => {address: user, balance: INIT_BALANCE}),
      validators' = VALIDATORS.mapBy(validator => {address: validator, stake: 0, bonded: USERS.mapBy(x => 0), unbonded: 0}),
      pos' = {posAccount: 0},
      lastTx' = { tag: "Init", result: true, user: "", validator:"", amount: 0}
    }

    /*
    * Execution of the state machine.
    * 1. Pick a random amount from 0 to MAX_UINT, a user and a validator
    * 2. Execute one of the actions: delegate, unbond or withdraw.
    */
    action step: bool = {
        nondet user = USERS.oneOf()
        nondet validator = VALIDATORS.oneOf()
        nondet amount = 0.to(MAX_UINT).oneOf()
        // Execute one of the available actions/methods
        any {
            actionDelegate(user, validator, amount),
            actionUnbond(user, validator, amount),
            actionWithdraw(user, validator)
        }
    }

    /*
    * The test testDelegate is a unitest-like test for the delegate function.
    */
    run testDelegateTest = {
      val delegatorState = {address: "bob", balance: INIT_BALANCE}
      val validatorState = {address: "alice", stake: 0, bonded: USERS.mapBy(x => 0), unbonded: 0}
      val posState = {posAccount: 0}
      nondet amount = 0.to(MAX_UINT).oneOf()
      val result = delegate(delegatorState, validatorState, posState, amount)
      if (result.success) {
        all {
          assert(result.delegatorState.balance == INIT_BALANCE - amount),
          assert(result.validatorState.bonded.get(delegatorState.address) == amount),
          assert(result.validatorState.unbonded == 0),
          assert(result.validatorState.stake == amount),
          assert(result.posState.posAccount == amount)
        }
      } else {
        all {
          assert(amount > INIT_BALANCE),
          assert(result.delegatorState == delegatorState),
          assert(result.validatorState == validatorState),
          assert(result.posState == posState)
        }
      }
    }
}