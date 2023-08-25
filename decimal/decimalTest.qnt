// -*- mode: Bluespec; -*-
module decimalTest {
    import decimal.* from "./decimal"

    var opcode: str
    var opArg1: Dec
    var opArg2: Dec
    var opResult: Dec

    action init = any {
        // try various constructors
        {
            nondet i64 = (-2^63).to(2^63 - 1).oneOf()
            mkWholeDec("newDec", i64, newDec)
        },
        {
            nondet i64 = (-2^63).to(2^63 - 1).oneOf()
            nondet f64 = (-2^63).to(2^63 - 1).oneOf()
            mkFracDec("newDecWithPrec", i64, f64, newDecWithPrec)
        },
        {
            nondet i256 = (-2^256).to(2^256).oneOf()
            mkWholeDec("newDecFromInt", i256, newDecFromInt)
        },
        {
            nondet i256 = (-2^256).to(2^256).oneOf()
            nondet f64 = (-2^63).to(2^63 - 1).oneOf()
            mkFracDec("newDecFromIntWithPrec", i256, f64, newDecFromIntWithPrec)
        },
        {
            nondet i = Int.oneOf()
            mkWholeDec("newDecFromBigInt", i, newDecFromBigInt)
        },
        {
            nondet i = Int.oneOf()
            nondet f64 = (-2^63).to(2^63 - 1).oneOf()
            mkFracDec("newDecFromBigIntWithPrec", i, f64, newDecFromBigIntWithPrec)
        },
    }

    // apply a unary operator
    action applyUnary(name: str, f: (Dec) => Dec): bool = {
        nondet i = 0.to(2^MAX_DEC_BIT_LEN).oneOf()
        pure val di: Dec = { error: false, value: i }
        all {
            isDec(i),
            opcode' = name,
            opArg1' = di,
            opArg2' = newDecFromInt(0),
            opResult' = f(di),
       }
    }

    // apply a binary operator
    action applyBinary(name: str, f: (Dec, Dec) => Dec): bool = {
        nondet i = 0.to(2^MAX_DEC_BIT_LEN).oneOf()
        nondet j = 0.to(2^MAX_DEC_BIT_LEN).oneOf()
        pure val di: Dec = { error: false, value: i }
        pure val dj: Dec = { error: false, value: j }
        all {
            isDec(i),
            isDec(j),
            opcode' = name,
            opArg1' = di,
            opArg2' = dj,
            opResult' = f(di, dj),
       }
    }

    // construct a decimal provided whole and fractional parts
    action mkWholeDec(name: str, whole: int, f: (int) => Dec): bool = all {
        opcode' = name,
        opArg1' = { error: false, value: whole },
        opArg2' = { error: false, value: 0 },
        opResult' = f(whole),
    }

    // construct a decimal provided whole and fractional parts
    action mkFracDec(name: str, whole: int, frac: int, f: (int, int) => Dec): bool = all {
        opcode' = name,
        opArg1' = { error: false, value: whole },
        opArg2' = { error: false, value: frac },
        opResult' = f(whole, frac),
    }

    action step = any {
        // try operations
        applyBinary("add", add),
        applyBinary("sub", sub),
        applyBinary("mul", mul),
        applyBinary("quo", quo),
        applyBinary("quoTruncate", quoTruncate),
        applyBinary("mulTruncate", mulTruncate),
        applyUnary("ceil", ceil),
        applyUnary("chopPrecisionAndRound", chopPrecisionAndRound),
    }

    // check this to produce an operation that results in error
    def noError = not(opResult.error)

    // if no error is reported, then the result is a proper a decimal
    def isDecWhenNoError =
        not(opResult.error) implies isDec(opResult.value)
    
    // if the result is a proper decimal, then no error is reported
    // (unless there is a division by zero)
    def noErrorWhenIsDec =
        isDec(opResult.value) implies or {
          not(opResult.error),
          opcode == "quo" and opArg2.value == 0,
          opcode == "quoTruncate" and opArg2.value == 0,
        }
}