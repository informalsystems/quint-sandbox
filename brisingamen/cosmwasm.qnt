/// A few definitions that partially capture the Cosmwasm environment.
module cosmwasm {
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
}