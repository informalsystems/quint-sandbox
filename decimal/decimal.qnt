// -*- mode: Bluespec; -*-
/**
 * A specification of Cosmos SDK decimals (for v0.46.4).
 *
 * Igor Konnov, Informal Systems, 2022-2023.
 *
 * https://github.com/cosmos/cosmos-sdk/blob/v0.46.4/types/decimal.go
 */

module decimal {
    import aux.* from "./aux"

    // The number of decimal places to the right of '.',
    // that is, in the FRACTIONAL part.
    pure val PRECISION = 18

    // precision multipliers for 0 to 18 digits to the right of '.'
    pure val precisionReuse = Map(
        18 -> 1,
        17 -> 10,
        16 -> 100,
        15 -> 1_000,
        14 -> 10_000,
        13 -> 100_000,
        12 -> 1_000_000,
        11 -> 10_000_000,
        10 -> 100_000_000,
        9 -> 1_000_000_000,
        8 -> 10_000_000_000,
        7 -> 100_000_000_000,
        6 -> 1_000_000_000_000,
        5 -> 10_000_000_000_000,
        4 -> 100_000_000_000_000,
        3 -> 1_000_000_000_000_000,
        2 -> 10_000_000_000_000_000,
        1 -> 100_000_000_000_000_000,
        0 -> 1_000_000_000_000_000_000
    )

    pure def getMultiplier(prec: int): int = {
        if (keys(precisionReuse).contains(prec))
            precisionReuse.get(prec)
        else
            1
    }

    /// The maximum number of bits to represent a decimal,
    /// up to 256 bits for the whole part and up to 59/60 bits for the digits
    /// to the right of '.'. This value is used in chopping.
    ///
    /// In cosmos-sdk v0.45.1, MAX_DEC_BIT_LEN == 316.
    /// In cosmos-sdk v0.46.4, MAX_DEC_BIT_LEN == 315.
    pure val MAX_DEC_BIT_LEN = 315

    // This is 1.00...00 with PRECISION digits afer '.' represented as an integer
    pure val ONE = 10^PRECISION

    // This is 0.500....00 with PRECISION digits after '.' represented as an integer
    pure val HALF = 5 * 10^(PRECISION - 1)

    /// A decimal is a record that contains two fields:
    ///
    ///  - error is the error flag which is true
    ///    iff the decimal number is considered invalid (e.g., on overflow);
    ///
    ///  - value is the math integer representing the decimal <intPart>.<fractionalPart> as
    ///    intPart * 10^ONE + fractionalPart.
    type Dec = { error: bool, value: int }

    // Go Int wraps big.Int with a 257 bit range bound
    // Checks overflow, underflow and division by zero
    // Exists in range from -(2^256 - 1) to 2^256 - 1.

    // Does a big integer represent a Cosmos decimal?
    // This invariant is not enforced in the code.
    // Some of the code is checking isBitLenOk instead.
    pure def isDec(bigint: int): bool = and {
        // the whole part fits into 256 bits (ignoring the sign)
        abs(bigint / ONE) < 2^256,
        // the fractional part represents up to PRECISION digits
        abs(bigint % ONE) < 10^PRECISION,
    }

    /// This is the invariant that is often checked in the code:
    /// Whether the big integer fits into a number of bits (315 or 316).
    pure def isBitLenOk(bigint: int): bool = {
        abs(bigint) < 2^MAX_DEC_BIT_LEN
    }

    /// Construct a decimal representation of a 64-bit integer, all digits are
    /// copied to the left of '.'.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDec(123)
    ///    { error: false, value: 123000000000000000000 }
    /// ```
    pure def newDec(int64: int): Dec = {
        {
            error: not(isInt64(int64)),
            value: int64 * ONE
        }
    }

    /// Construct a decimal from a 64-bit integer `i` by specifying the number
    /// of digits to the right of '.'. When `prec64 == 0`, `i` is copied to
    /// to the left of '.'. When `prec64 == 0`, the last 18 digits of `i` are
    /// copied to the right of '.', and the other digits are copied to the left
    /// of '.'.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDecWithPrec(123, 0)
    ///    { error: false, value: 123000000000000000000 }
    ///    >>> newDecWithPrec(123, 18)
    ///    { error: false, value: 123 }
    /// ```
    pure def newDecWithPrec(i: int, prec64: int): Dec = {
        {
            error: not(isInt64(i)) or prec64 < 0 or prec64 > PRECISION,
            value: i * getMultiplier(prec64)
        }
    }

    /// Construct a decimal from a Golang big.Int. The integer is simply
    /// copied to the left of '.'
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDecFromInt(123)
    ///    { error: false, value: 123000000000000000000 }
    /// ```
    pure def newDecFromBigInt(i: int): Dec = {
        { error: false, value: i * ONE }
    }

    /// Construct a decimal from a Golang big.Int by specifying the number
    /// of digits to the right of '.'. When `prec64 == 0`, `i` is copied to
    /// to the left of '.'. When `prec64 == 0`, the last 18 digits of `i` are
    /// copied to the right of '.', and the other digits are copied to the left
    /// of '.'.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDecFromBigIntWithPrec(123, 0)
    ///    { error: false, value: 123000000000000000000 }
    ///    >>> newDecFromBigIntWithPrec(123, 18)
    ///    { error: false, value: 123 }
    /// ```
    pure def newDecFromBigIntWithPrec(i: int, prec64: int): Dec = {
        {
            error: prec64 < 0 or prec64 > PRECISION,
            value: i * getMultiplier(prec64)
        }
    }

    /// Construct a decimal from a sdkmath.Int. The integer is simply
    /// copied to the left of '.'
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDecFromInt(123)
    ///    { error: false, value: 123000000000000000000 }
    /// ```
    pure def newDecFromInt(i: int): Dec = {
        { error: not(isSdkInt(i)), value: i * ONE }
    }

    /// Construct a decimal from a sdkmath.Int by specifying the number
    /// of digits to the right of '.'. When `prec64 == 0`, `i` is copied to
    /// to the left of '.'. When `prec64 == 0`, the last 18 digits of `i` are
    /// copied to the right of '.', and the other digits are copied to the left
    /// of '.'.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> newDecFromIntWithPrec(123, 0)
    ///    { error: false, value: 123000000000000000000 }
    ///    >>> newDecFromIntWithPrec(123, 18)
    ///    { error: false, value: 123 }
    /// ```
    pure def newDecFromIntWithPrec(i: int, prec64: int): Dec = {
        {
            error: not(isSdkInt(i)) or prec64 < 0 or prec64 > PRECISION,
            value: i * getMultiplier(prec64)
        }
    }

    /// Add y to x.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> add({ error: false, value: 123_300000_000000_000019 },
    ///            { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 128300000000000000019 }
    /// ```
    pure def add(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else {
            pure val sum: int = x.value + y.value
            { error: not(isBitLenOk(sum)), value: sum }
        }
    }

    /// Subtract y from x.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> sub({ error: false, value: 123_300000_000000_000019 },
    ///            { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 118300000000000000019 }
    /// ```
    pure def sub(x: Dec, y: Dec): Dec = {
        add(x, { ...y, value: -y.value })
    }

    /// Divides the sdk.Dec number with sdk.Int number and returns sdk.Dec number
    /// but only the truncated part (unlike the QuoRem, which returns the whole
    /// number, and the remainder) - it implements food division.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> quoInt({ error: false, value: 123_300000_000000_000019 },
    ///               5_000000_000000_000000)
    ///    { error: false, value: 24 }
    /// ```
    pure def quoInt(x: Dec, y: int): Dec = {
        if (x.error) {
            // propagate the error
            x
        } else if (y == 0) {
            // division by zero
            { error: true, value: x.value }
        } else {
            // use absolute values, as integer division behaves differently on
            // negative numbers in different languages
            pure val absResult: int = abs(x.value) / abs(y)
            pure val isNeg: bool = or {
                x.value < 0 and y > 0,
                x.value > 0 and y < 0,
            }
            {
                error: false,
                value: if (isNeg) -absResult else absResult
            }
        }
    }

    /// Quotient of dividing x by y.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> quo({ error: false, value: 123_300000_000000_000019 },
    ///            { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 24660000000000000004 }
    /// ```
    pure def quo(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else if (y.value == 0) {
            { error: true, value: 0 }
        } else {
            pure val quoX: int = (x.value * ONE * ONE) / y.value
            pure val chopped: int = chopPrecisionAndRound(quoX)
            { error: not(isBitLenOk(chopped)), value: chopped }
        }
    }

    /// Quotient of dividing x by y, truncated.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> quoTruncate({ error: false, value: 123_300000_000000_000019 },
    ///                    { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 24660000000000000003 }
    /// ```
    pure def quoTruncate(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else if (y.value == 0) {
            { error: true, value: 0 }
        } else {
            pure val quoX: int = (x.value * ONE * ONE) / y.value
            // chopPrecisionAndTruncate
            pure val chopped: int = quoX / ONE
            { error: not(isBitLenOk(chopped)), value: chopped }
        }
    }

    /// Quotient of dividing x by y, rounded up.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> quoRoundup({ error: false, value: 123_300000_000000_000019 },
    ///            { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 24660000000000000004 }
     /// ```
    pure def quoRoundup(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else if (y.value == 0) {
            { error: true, value: 0 }
        } else {
            pure val quoX: int = (x.value * ONE * ONE) / y.value
            pure val chopped: int = chopPrecisionAndRoundUp(quoX)
            { error: not(isBitLenOk(chopped)), value: chopped }
        }
    }

    /// Decimal multiplication: x is precisely multiplied by y,
    /// then the last 18 digits after '.' are removed by rounding
    /// to the even number at half.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> mul({ error: false, value: 123_300000_000000_000000 },
    ///            { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 616500000000000000000 }
    /// ```
    pure def mul(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else {
            // the perfect math product of two integers, which we have to round
            pure val mathProd: int = x.value * y.value
            pure val chopped: int = chopPrecisionAndRound(mathProd)
            // equivalent to absResult.BitLen() > maxDecBitLen of Golang
            { error: not(isBitLenOk(chopped)), value: chopped }
        }
    }

    /// Multiplicate x by y and truncate the excess digits, that is,
    /// over 18 digits to the right of '.'.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> mulTruncate({ error: false, value: 123_300000_000000_000000 },
    ///                     { error: false, value: 5_000000_000000_000000 })
    ///    { error: false, value: 616500000000000000000 }
    /// ```
    pure def mulTruncate(x: Dec, y: Dec): Dec = {
        pure val mathProd: int = x.value * y.value
        // chopPrecisionAndTruncate
        pure val chopped: int = mathProd / ONE
        { error: not(isBitLenOk(chopped)), value: chopped }
    }

    /// Multiply a decimal x by a big integer i.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> mulInt({ error: false, value: 123_300000_000000_000000 }, 5)
    ///    { error: false, value: 616500000000000000000 }
    /// ```
    pure def mulInt(x: Dec, i: int): Dec = {
        pure def prod = x.value * i
        { error: not(isBitLenOk(prod)), value: prod }
    }

    /// Remove a PRECISION amount of rightmost digits and perform bankers rounding
    ///  on the remainder (gaussian rounding) on the digits which have been removed.
    /// 
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> chopPrecisionAndRound(123_000000_000000_000000)
    ///    123
    /// ```
    pure def chopPrecisionAndRound(x: int): int = {
        pure val absX: int = abs(x)
        // the integer part, that is, the digits to the left of '.'
        pure val quoX: int = absX / ONE
        // the fractional part, that is, the digits to the right of '.'
        pure val remX: int = absX % ONE
        if (remX == 0) {
            if (x >= 0) quoX else -quoX
        } else {
            pure val absResult: int =
                // when at half precisely, use bankers rounding:
                // round up to the even number
                if (remX < HALF or (remX == HALF and quoX % 2 == 0)) {
                    quoX
                } else {
                    quoX + 1
                }
            if (x >= 0) absResult else -absResult
        }
}

    /// Remove a PRECISION amount of rightmost digits and round to the smallest larger integer,
    /// unless the remainder is 0.
    /// 
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> chopPrecisionAndRoundUp({ error: false, value: 123_000000_000000_000017 })
    ///    124
    ///    >>> chopPrecisionAndRoundUp({ error: false, value: -123_000000_000000_000017 })
    ///    -123
     /// ```
    pure def chopPrecisionAndRoundUp(x: int): int = {
        pure val absX: int = abs(x)
        if (x < 0) {
            // A negative number is getting rounded to 0, chopPrecisionAndTruncate
            -(absX / ONE)
        } else {
            // the integer part, that is, the digits to the left of '.'
            pure val quoX: int = absX / ONE
            // the fractional part, that is, the digits to the right of '.'
            pure val remX: int = absX % ONE
            if (remX == 0) {
                quoX 
            } else {
                quoX + 1
            }
        }
    }

    /// Ceil returns the smallest integer value (as a decimal) that is greater than
    /// or equal to the given decimal.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> ceil({ error: false, value: 123_300000_000000_000000 })
    ///    { error: false, value: 124000000000000000000 }
    /// ```
    pure def ceil(x: Dec): Dec = {
        if (x.error) {
            x
        } else {
            pure val value: int =   
                if (x.value % ONE == 0 or x.value < 0) {
                    (x.value / ONE) * ONE
                } else {
                    ((x.value / ONE) + 1) * ONE
                }
            { error: false, value: value }
        }
    }

    /// RoundInt round the decimal using bankers rounding.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> roundInt(123_345000_000000_000000)
    ///    123
    /// ```
    pure def roundInt(x: Dec): int = chopPrecisionAndRound(x.value)

    /// Compute `base^degree`, assuming that pow64 is a 64-bit unsigned integer.
    ///
    /// ```
    ///    $ quint -r decimal.qnt::decimal
    ///    >>> power({ error: false, value: 123_300000_000000_000000 }, 11)
    ///    { error: false, value: 100136830829095253843725566020536170000000 }
    /// ```
    pure def power(base: Dec, degree: int): Dec = {
        if (degree <= 0) {
            { error: false, value: 1 }
        } else {
            // Since a 64-bit integer can be divided by 2 up to 64 times,
            // we bound the number of iterations by 64.
            // We carry three loop variables in a record: d, tmp, and power.
            pure val loopResult =
                range(0, 64).foldl({ d: base, tmp: { error: false, value: ONE }, power: degree },
                (s, i) => {
                    if (s.power <= 1) {
                        // the loop has terminated
                        s
                    } else {
                        pure val newTmp = if (s.power % 2 != 0) s.tmp.mul(s.d) else s.tmp
                        { tmp: newTmp, d: s.d.mul(s.d), power: s.power / 2 }
                    }
               }
            )
            loopResult.d.mul(loopResult.tmp)
        }
    }

    // TODO: approxRoot
}