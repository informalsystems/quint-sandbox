// -*- mode: Bluespec; -*-
module decimalTest {
    import decimal.* from "./decimal"

    var opcode: str
    var opArg1: Dec
    var opArg2: Dec
    var opResult: Dec

    action init = any {
        // try various constructors
        initNewDec,
        initNewDecWithPrec,
        initNewDecFromInt,
        initNewDecFromIntWithPrec,
        initNewDecFromBigInt,
        initNewDecFromBigIntWithPrec,
    }

    action step = any {
        // Try one of the operations.
        // Note that some of the operations are tough arithmetic,
        // which is hard both for humans and constraint solvers.
        stepAdd,
        stepSub,
        stepMul,
        stepMulTruncate,
        stepQuo,
        stepQuoTruncate,
        stepQuoRoundup,
        stepCeil,
        stepRoundInt,
     }

    // six ways to construct a Dec
    action initNewDec = {
        nondet i64 = (-2^63).to(2^63 - 1).oneOf()
        mkWholeDec("newDec", i64, newDec)
    }

    action initNewDecWithPrec = {
        nondet i64 = (-2^63).to(2^63 - 1).oneOf()
        nondet f64 = 0.to(PRECISION).oneOf()
        mkFracDec("newDecWithPrec", i64, f64, newDecWithPrec)
    }

    action initNewDecFromInt = {
        nondet i256 = (-2^256 + 1).to(2^256 - 1).oneOf()
        mkWholeDec("newDecFromInt", i256, newDecFromInt)
    }

    action initNewDecFromIntWithPrec = {
        nondet i256 = (-2^256 + 1).to(2^256 - 1).oneOf()
        nondet f64 = 0.to(PRECISION).oneOf()
        mkFracDec("newDecFromIntWithPrec", i256, f64, newDecFromIntWithPrec)
    }

    action initNewDecFromBigInt = {
        nondet i = Int.oneOf()
        mkWholeDec("newDecFromBigInt", i, newDecFromBigInt)
    }

    action initNewDecFromBigIntWithPrec = {
        nondet i = Int.oneOf()
        nondet f64 = 0.to(PRECISION).oneOf()
        mkFracDec("newDecFromBigIntWithPrec", i, f64, newDecFromBigIntWithPrec)
    }

    // apply a unary operator
    action applyUnary(name: str, f: (Dec) => Dec): bool = {
        nondet whole = (-2^256 + 1).to(2^256 - 1).oneOf()
        nondet frac = (-10^18 + 1).to(10^18 - 1).oneOf()
        pure val d: Dec = { error: false, value: whole * ONE + frac }
        all {
            isBitLenOk(d.value),
            opcode' = name,
            opArg1' = d,
            opArg2' = newDecFromInt(0),
            opResult' = f(d),
       }
    }

    // apply a binary operator
    action applyBinary(name: str, f: (Dec, Dec) => Dec): bool = {
        nondet whole1 = (-2^256 + 1).to(2^256 - 1).oneOf()
        nondet frac1 = (-10^18 + 1).to(10^18 - 1).oneOf()
        nondet whole2 = (-2^256 + 1).to(2^256 - 1).oneOf()
        nondet frac2 = (-10^18 + 1).to(10^18 - 1).oneOf()
        pure val d1: Dec = { error: false, value: whole1 * ONE + frac1 }
        pure val d2: Dec = { error: false, value: whole2 * ONE + frac2 }
        all {
            isBitLenOk(d1.value),
            isBitLenOk(d2.value),
            opcode' = name,
            opArg1' = d1,
            opArg2' = d2,
            opResult' = f(d1, d2),
       }
    }

    action stepCeil = applyUnary("ceil", ceil)

    action stepRoundInt =
        applyUnary("roundInt", (i => { error: false, value: roundInt(i) }))

    action stepAdd = applyBinary("add", add)

    action stepSub = applyBinary("sub", sub)

    action stepMul = applyBinary("mul", mul)

    action stepQuo = applyBinary("quo", quo)

    action stepQuoTruncate = applyBinary("quoTruncate", quoTruncate)

    action stepQuoRoundup = applyBinary("quoRoundUp", quoRoundup)

    action stepMulTruncate = applyBinary("mulTruncate", mulTruncate)

    action stepPower = {
        nondet whole1 = (-2^256 + 1).to(2^256 - 1).oneOf()
        nondet frac1 = (-10^18 + 1).to(10^18 - 1).oneOf()
        nondet pow64 = (-1).to(2^64 - 1).oneOf()
        pure val d1: Dec = { error: false, value: whole1 * ONE + frac1 }
        all {
            isBitLenOk(d1.value),
            opcode' = "power",
            opArg1' = d1,
            opArg2' = { error: false, value: pow64 },
            opResult' = power(d1, pow64),
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

    // check this to produce an operation that results in error
    val noError = not(opResult.error)

    // if no error is reported, then the result is a proper a decimal
    val isDecWhenNoError =
        not(opResult.error) implies isDec(opResult.value)

    // if no error is reported, then the decimal fits into MAX_DEC_BIT_LEN
    val bitLenOkWhenNoError =
        not(opResult.error) implies isBitLenOk(opResult.value)

    // If no error is reported, then the decimal fits into MAX_DEC_BIT_LEN.
    // Restricted to unary and binary operators, but not constructors,
    // which violate bitLenOkWhenNoError.
    val bitLenOkWhenNoErrorNoCtor = or {
        not(Set("add", "sub", "mul", "quo", "quoRoundup",
                "quoTruncate", "mulTruncate", "ceil", "roundInt")
            .contains(opcode)),
        bitLenOkWhenNoError,
    }
      
    // If the result is a proper decimal, then no error is reported
    // (unless there is a division by zero).
    // This does not hold true for v0.46.4.
    val noErrorWhenIsDec =
        isDec(opResult.value) implies or {
          not(opResult.error),
          opcode == "quo" and opArg2.value == 0,
          opcode == "quoTruncate" and opArg2.value == 0,
          opcode == "quoRoundup" and opArg2.value == 0,
        }

    // TODO: check power
}