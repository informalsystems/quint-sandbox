use num_bigint::BigInt;
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Clone, Debug, Deserialize)]
pub struct Investment {
    pub owner: String,
    pub amount: BigInt,
}

#[derive(Clone, Debug, Deserialize)]
pub struct BankState {
    pub balances: HashMap<String, BigInt>,
    pub investments: HashMap<BigInt, Investment>,
    pub next_id: BigInt,
}

pub fn deposit(bank_state: &mut BankState, depositor: String, amount: BigInt) -> Option<String> {
    if amount <= BigInt::from(0) {
        return Some("Amount should be greater than zero".to_string());
    }

    bank_state
        .balances
        .entry(depositor)
        .and_modify(|curr| *curr += amount);
    None
}

pub fn withdraw(bank_state: &mut BankState, withdrawer: String, amount: BigInt) -> Option<String> {
    if amount <= BigInt::from(0) {
        return Some("Amount should be greater than zero".to_string());
    }

    if bank_state.balances.get(&withdrawer).unwrap() < &amount {
        return Some("Balance is too low".to_string());
    }

    bank_state
        .balances
        .entry(withdrawer)
        .and_modify(|curr| *curr -= amount);
    None
}

pub fn transfer(
    bank_state: &mut BankState,
    sender: String,
    receiver: String,
    amount: BigInt,
) -> Option<String> {
    if amount <= BigInt::from(0) {
        return Some("Amount should be greater than zero".to_string());
    }

    if bank_state.balances.get(&sender).unwrap() < &amount {
        return Some("Balance is too low".to_string());
    }

    bank_state
        .balances
        .entry(sender)
        .and_modify(|curr| *curr -= amount.clone());
    bank_state
        .balances
        .entry(receiver)
        .and_modify(|curr| *curr += amount);
    None
}

pub fn buy_investment(bank_state: &mut BankState, buyer: String, amount: BigInt) -> Option<String> {
    if amount <= BigInt::from(0) {
        return Some("Amount should be greater than zero".to_string());
    }

    if bank_state.balances.get(&buyer).unwrap() < &amount {
        return Some("Balance is too low".to_string());
    }

    bank_state
        .balances
        .entry(buyer.clone())
        .and_modify(|curr| *curr -= amount.clone());

    bank_state.investments.insert(
        bank_state.next_id.clone(),
        Investment {
            owner: buyer,
            amount,
        },
    );

    bank_state.next_id += 1;
    None
}

pub fn sell_investment(
    bank_state: &mut BankState,
    seller: String,
    investment_id: BigInt,
) -> Option<String> {
    if let Some(investment) = bank_state.investments.get(&investment_id) {
        if investment.owner != seller {
            return Some("Seller can't sell an investment they don't own".to_string());
        }
        bank_state
            .balances
            .entry(seller)
            .and_modify(|curr| *curr += investment.amount.clone());
        // bank_state.investments.remove(&investment_id);
        return None;
    }
    Some("No investment with this id".to_string())
}
