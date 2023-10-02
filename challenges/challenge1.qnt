/**
 * A Quint challenge after:
 * https://blog.openzeppelin.com/pods-finance-ethereum-volatility-vault-audit-1
 */
module challenge1 {
  /// a 256-bit integer
  type Uint = int
  // The standard behavior of wrapping up on overflow,
  // e.g., when used inside unchecked { ... } in Solidity.
  pure def toUint(i: int): Uint = i % (2^256)

  /// Multiply x by y, then divide by denominator, rounding up
  pure def mulDivUp(x: Uint, y: Uint, denominator: Uint): (Uint, str) = {
    if (denominator == 0) {
      /* return */ (denominator, "DivByZero")
    } else {
      pure val numerator = toUint(x * y)
      pure val result =
        toUint(numerator / denominator)
          + (if (numerator % denominator > 0) 1 else 0)
      /* return */ (result, "")
    }
  }
}