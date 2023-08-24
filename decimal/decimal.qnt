// -*- mode: Bluespec; -*-

/**
 * A specification of Cosmos SDK decimals.
 *
 * Igor Konnov, Informal Systems, 2022-2023.
 *
 * https://github.com/cosmos/cosmos-sdk/blob/v0.46.4/types/decimal.go
 */
module decimal {
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
    pure val MAX_DEC_BIT_LEN = 315

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

    /**
     * Return the absolute value of a math integer.
     */
    pure def abs(x: int): int = {
        if (x >= 0) x else -x
    }

    // Go Int wraps big.Int with a 257 bit range bound
    // Checks overflow, underflow and division by zero
    // Exists in range from -(2^256 - 1) to 2^256 - 1.

    // Can an integer represent a Cosmos decimal?
    pure def isDec(bigint: int): bool = {
        abs(bigint) <= (2^256 - 1) * ONE + 10^(PRECISION + 1) - 1
    }

    /**
     * Converts Int number to sdk.Dec number.
     * Creates new decimal number from big integer assuming WHOLE numbers.
     */
    pure def toDec(bigint: int): Dec = {
        {
            error: not(isDec(bigint * ONE)),
            value: bigint * ONE
        }
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
            { error: abs(sum) >= 2^MAX_DEC_BIT_LEN, value: sum }
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
            { ...chopped, error: abs(chopped.value) >= 2^MAX_DEC_BIT_LEN }
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
            { error: abs(chopped) >= 2^MAX_DEC_BIT_LEN, value: chopped }
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
            { error: abs(chopped.value) >= 2^MAX_DEC_BIT_LEN, value: chopped.value }
        }
    }

    /**
     * Multiplication truncated.
     */
    pure def mulTruncate(x: Dec, y: Dec): Dec = {
        pure val chopped = x.mul(y).quoInt(ONE)
        { error: abs(chopped.value) >= 2^MAX_DEC_BIT_LEN, value: chopped.value }
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