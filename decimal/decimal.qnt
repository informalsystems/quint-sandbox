// -*- mode: Bluespec; -*-

// auxilliary definitions that do not belong to the original code
module aux {
    /**
     * Return the absolute value of a math integer.
     */
    pure def abs(x: int): int = {
        if (x >= 0) x else -x
    }

    /**
     * Does an integer fit into 64 bits?
     */
    pure def isInt64(i: int): bool = {
        i >= -2^63 and i < 2^63
    }

    /**
     * Does an integer fit into a Golang Int?
     */
    pure def isGoInt(i: int): bool = {
        abs(i) < 2^256
    }
}

/**
 * A specification of Cosmos SDK decimals.
 *
 * Igor Konnov, Informal Systems, 2022-2023.
 *
 * https://github.com/cosmos/cosmos-sdk/blob/v0.46.4/types/decimal.go
 */
module decimal {
    import aux.*

    // The number of decimal places to the right of '.',
    // that is, in the FRACTIONAL part.
    pure val PRECISION = 18

    /**
     * The maximum number of bits to represent a decimal,
     * up to 256 bits for the whole part and up to 59/60 bits for the digits after '.'.
     * This value is used in chopping.
     *
     * In cosmos-sdk v0.45.1, MAX_DEC_BIT_LEN == 316.
     * In cosmos-sdk v0.46.4, MAX_DEC_BIT_LEN == 315.
     */
    pure val MAX_DEC_BIT_LEN = 316

    // This is 1.00...00 with PRECISION digits afer '.' represented as an integer
    pure val ONE = 10^PRECISION

    // This is 0.500....00 with PRECISION digits after '.' represented as an integer
    pure val HALF = 5 * 10^(PRECISION - 1)

    /**
     * A decimal is a record that contains two fields:
     *
     *  - error is the error flag which is true
     *    iff the decimal number is considered invalid (e.g., overflow);

     *  - value is the math integer representing the decimal intPart.fractionalPart as
     *    intPart * 10^PRECISION + fractionalPart.
     */
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

    // This is the invariant that is often checked in the code.
    // You may notice that isDec is not equivalent to isBitLenOk.
    pure def isBitLenOk(bigint: int): bool = {
        abs(bigint) < 2^MAX_DEC_BIT_LEN
    }

    /**
     * Construct a decimal representation of a 64-bit integer.
     * In Golang, this operator always succeeds.
     * Our definition fails if int64 does not fit into 64 bits.
     */
    pure def newDec(int64: int): Dec = {
        {
            error: not(isInt64(int64)),
            value: int64 * ONE
        }
    }

    /**
     * Construct a decimal representation of whole and fractional parts.
     * In Golang, this operator always succeeds.
     * Our definition fails if int64 or prec64 do not fit into 64 bits.
     */
    pure def newDecWithPrec(int64: int, prec64: int): Dec = {
        {
            error: not(isInt64(int64)) or not(isInt64(prec64)),
            value: int64 * ONE + prec64
        }
    }

    /**
     * Construct a whole decimal from bigint.
     */
    pure def newDecFromBigInt(i: int): Dec = {
        { error: false, value: i * ONE }
    }

    /**
     * Construct a decimal with a whole part represented with
     * bigint and a fractional part represented with a 64-bit integer.
     */
    pure def newDecFromBigIntWithPrec(whole: int, prec64: int): Dec = {
        { error: not(isInt64(prec64)), value: whole * ONE + prec64}
    }

    /**
     * Construct a whole decimal from a golang math.Int.
     */
    pure def newDecFromInt(i: int): Dec = {
        { error: not(isGoInt(i)), value: i * ONE }
    }

    /**
     * Construct a decimal from a golang math.Int and a 64-bit integer.
     */
    pure def newDecFromIntWithPrec(i: int, prec64: int): Dec = {
        { error: not(isGoInt(i)) or not(isInt64(prec64)), value: i * ONE + prec64 }
    }

    /**
     * Add x to y.
     */
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

    /**
     * Subtract y from x.
     */
    pure def sub(x: Dec, y: Dec): Dec = {
        add(x, { error: y.error, value: -y.value })
    }

    /**
     * Divides the sdk.Dec number with sdk.Int number and returns sdk.Dec number
     * but only the truncated part (unlike the QuoRem, which returns the whole
     * number, and the remainder) - it implements food division.
     */
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

    /**
     * Quotient of dividing x by y, rounded up.
     */
    pure def quo(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else if (y.value == 0) {
            { error: true, value: 0 }
        } else {
            pure val quoX = (x.value * PRECISION * PRECISION) / y.value
            pure val chopped = chopPrecisionAndRound({ error: false, value: quoX })
            { ...chopped, error: not(isBitLenOk(chopped.value)) }
        }
    }

    /**
     * Quotient of dividing x by y, truncated.
     */
    pure def quoTruncate(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else if (y.value == 0) {
            { error: true, value: 0 }
        } else {
            pure val quoX = (x.value * PRECISION * PRECISION) / y.value
            pure val chopped = quoX / PRECISION
            { error: not(isBitLenOk(chopped)), value: chopped }
        }
    }

    /**
     * Quotient of dividing x by y, rounded up.
     */
    pure def quoRoundup(x: Dec, y: Dec): Dec = x.quo(y)

    /**
     * Decimal multiplication.
     */
    pure def mul(x: Dec, y: Dec): Dec = {
        if (x.error) {
            x
        } else if (y.error) {
            y
        } else {
            // the perfect math product of two integers, which we have to round
            pure val mathProd: int = x.value * y.value
            pure val chopped: Dec =
                chopPrecisionAndRound({ error: false, value: mathProd })
            // equivalent to absResult.BitLen() > maxDecBitLen of Golang
            { error: not(isBitLenOk(chopped.value)), value: chopped.value }
        }
    }

    /**
     * Multiplication truncated.
     */
    pure def mulTruncate(x: Dec, y: Dec): Dec = {
        pure val chopped = x.mul(y).quoInt(ONE)
        { error: not(isBitLenOk(chopped.value)), value: chopped.value }
    }

    /**
     * Multiply a decimal x by an integer i.
     */
    pure def mulInt(x: Dec, i: int): Dec = {
        mul(x, { error: false, value: i })
    }

    /**
     * Remove a PRECISION amount of rightmost digits and perform bankers rounding
     * on the remainder (gaussian rounding) on the digits which have been removed.
     */
    pure def chopPrecisionAndRound(x: Dec): Dec = {
        if (x.error) {
            x // propagate the error
        } else {
            pure val absX: int = abs(x.value)
            // the integer part, that is, the digits to the left of '.'
            pure val quoX: int = absX / ONE
            // the fractional part, that is, the digits to the right of '.'
            pure val remX: int = absX % ONE
            pure val absResult: int =
                // when at half precisely, use bankers rounding:
                // round up to the even number
                if (remX < HALF or (remX == HALF and quoX % 2 == 0)) {
                    quoX
                } else {
                    quoX + 1
                }
            {
                error: false,
                value: if (x.value >= 0) absResult else -absResult
            }
        }
    }

    /**
     * Ceil returns the smallest integer value (as a decimal) that is greater than
     * or equal to the given decimal.
     */
    pure def ceil(x: Dec): Dec = {
        if (x.error) {
            x
        } else {
            pure val value: int =   
                if (x.value % ONE == 0 or x.value < 0) {
                    x.value
                } else {
                    ((x.value / ONE) + 1) * ONE
                }
            { error: false, value: value }
        }
    }

    /**
     * RoundInt round the decimal using bankers rounding.
     */
    pure def roundInt(x: Dec): Dec = chopPrecisionAndRound(x)

    // TODO: power
    // TODO: approxRoot
}