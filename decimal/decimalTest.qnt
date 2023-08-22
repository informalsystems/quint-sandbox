module decimalTest {
    import decimal.* from "./decimal"

    var opcode: str
    var opArg1: Dec
    var opArg2: Dec
    var opResult: Dec

    action init = all {
        opcode' = "nop",
        opArg1' = toDec(0),
        opArg2' = toDec(0),
        opResult' = toDec(0),
    }

    // apply a unary operator
    action applyUnary(name: str, f: (Dec) => Dec): bool = {
        nondet i = 0.to(2^MAX_DEC_BIT_LEN).oneOf()
        pure val di: Dec = { error: false, value: i }
        all {
            isDec(i),
            opcode' = name,
            opArg1' = di,
            opArg2' = toDec(0),
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

    action step = any {
        applyBinary("add", add),
        applyBinary("sub", sub),
        applyBinary("mul", mul),
        applyBinary("quo", quo),
        applyBinary("quoTruncate", quoTruncate),
        applyBinary("mulTruncate", mulTruncate),
        applyUnary("ceil", ceil),
        applyUnary("chopPrecisionAndRound", chopPrecisionAndRound),
    }

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