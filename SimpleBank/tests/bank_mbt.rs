#[cfg(test)]
pub mod tests {
    use itf::de::{self, As};
    use itf::trace_from_str;
    use num_bigint::BigInt;
    use serde::Deserialize;
    use std::fs;
    use SimpleBank::bank::*;

    #[derive(Clone, Debug, Deserialize)]
    pub struct NondetPicks {
        #[serde(with = "As::<de::Option::<_>>")]
        pub depositor: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub withdrawer: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub sender: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub receiver: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub amount: Option<BigInt>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub buyer: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub seller: Option<String>,

        #[serde(with = "As::<de::Option::<_>>")]
        pub id: Option<BigInt>,
    }

    #[derive(Clone, Debug, Deserialize)]
    pub struct State {
        pub bank_state: BankState,

        #[serde(with = "As::<de::Option::<_>>")]
        pub error: Option<String>,

        #[serde(rename = "mbt::actionTaken")]
        pub action_taken: String,
        #[serde(rename = "mbt::nondetPicks")]
        pub nondet_picks: NondetPicks,
    }

    fn compare_error(trace_error: Option<String>, app_error: Option<String>) {
        if trace_error.is_some() {
            assert!(
                app_error.is_some(),
                "Expected action to fail with error: {:?}, but it succeeded",
                trace_error
            );
            println!("Action failed as expected");
        } else {
            assert!(
                app_error.is_none(),
                "Expected action to succeed, but it failed with error: {:?}",
                app_error
            );
            println!("Action successful as expected");
        }
    }

    #[test]
    fn model_test() {
        for i in 0..10000 {
            println!("Trace #{}", i);
            let data = fs::read_to_string(format!("traces/out{}.itf.json", i)).unwrap();
            let trace: itf::Trace<State> = trace_from_str(data.as_str()).unwrap();

            let mut bank_state = trace.states[0].value.bank_state.clone();

            for state in trace.states {
                let action_taken = state.value.action_taken;
                let nondet_picks = state.value.nondet_picks;

                match action_taken.as_str() {
                    "init" => {
                        println!("initializing");
                    }
                    "deposit_action" => {
                        let depositor = nondet_picks.depositor.clone().unwrap();
                        let amount = nondet_picks.amount.clone().unwrap();
                        println!("deposit({}, {})", depositor, amount);

                        let res = deposit(&mut bank_state, depositor, amount);
                        compare_error(state.value.error.clone(), res)
                    }
                    "withdraw_action" => {
                        let withdrawer = nondet_picks.withdrawer.clone().unwrap();
                        let amount = nondet_picks.amount.clone().unwrap();
                        println!("withdraw({}, {})", withdrawer, amount);

                        let res = withdraw(&mut bank_state, withdrawer, amount);
                        compare_error(state.value.error.clone(), res)
                    }
                    "transfer_action" => {
                        let sender = nondet_picks.sender.clone().unwrap();
                        let receiver = nondet_picks.receiver.clone().unwrap();
                        let amount = nondet_picks.amount.clone().unwrap();
                        println!("transfer({}, {}, {})", sender, receiver, amount);

                        let res = transfer(&mut bank_state, sender, receiver, amount);
                        compare_error(state.value.error.clone(), res)
                    }
                    "buy_investment_action" => {
                        let buyer = nondet_picks.buyer.clone().unwrap();
                        let amount = nondet_picks.amount.clone().unwrap();
                        println!("buy_investment({}, {})", buyer, amount);

                        let res = buy_investment(&mut bank_state, buyer, amount);
                        compare_error(state.value.error.clone(), res)
                    }
                    "sell_investment_action" => {
                        let seller = nondet_picks.seller.clone().unwrap();
                        let id = nondet_picks.id.clone().unwrap();
                        println!("sell_investment({}, {})", seller, id);

                        let res = sell_investment(&mut bank_state, seller, id);
                        compare_error(state.value.error.clone(), res)
                    }
                    action => panic!("Invalid action taken {}", action),
                }
            }
        }
    }
}
