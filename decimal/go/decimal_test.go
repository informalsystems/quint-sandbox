// A simple test driver that parses a test in the ITF format and executes it.
//
// A state of our testing state machine.
// Thanks to Ivan Gavran for demonstrating how ITF could be parsed in Golang.

package main

import (
	"fmt"
	"math/big"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/tidwall/gjson"
)

// a representation of a decimal in the test
type TestDec struct {
	// whether this decimal is malformed (a panic expected)
	error bool
	// the actual value that is represented as a big integer (integer + fractional)
	value big.Int
}

// a state of our testing state machine, which is also an input to the Golang test
type TestInput struct {
	opcode string
	arg1   TestDec
	arg2   TestDec
	result TestDec
}

// parse a big integer from ITF JSON
func parseBigInt(obj gjson.Result, target *big.Int) {
	var bigintStr = obj.Get("\\#bigint")
	if bigintStr.Exists() {
		_, ok := target.SetString(bigintStr.String(), 10)
		if !ok {
			panic(fmt.Errorf("expected a big.Int, found: %s", bigintStr.String()))
		}
	} else {
		target.SetInt64(obj.Int())
	}
}

// parse the states in the ITF JSON format, as produced from decimalTest.qnt
func parseItf(filename string) []TestInput {
	data, err := os.ReadFile(filename)
	if err != nil {
		panic(fmt.Errorf("error opening file: %v", err))
	}
	jsonStates := gjson.GetBytes(data, "states").Array()
	// iterate over all states of the test run
	var states = make([]TestInput, 0)
	for _, jsonState := range jsonStates {
		var state TestInput
		state.opcode = jsonState.Get("opcode").String()
		state.arg1.error = jsonState.Get("opArg1.error").Bool()
		state.arg2.error = jsonState.Get("opArg2.error").Bool()
		state.result.error = jsonState.Get("opResult.error").Bool()
		parseBigInt(jsonState.Get("opArg1.value"), &state.arg1.value)
		parseBigInt(jsonState.Get("opArg2.value"), &state.arg2.value)
		parseBigInt(jsonState.Get("opResult.value"), &state.result.value)
		states = append(states, state)
	}

	return states
}

// construct a Dec instance out of its pure integer representation
func bigintToDec(t *testing.T, i *big.Int) sdk.Dec {
	var abs big.Int
	// work with the absolute value but remember the sign of i
	abs.Abs(i)
	var s = abs.String()
	var sign string = ""
	if i.Sign() < 0 {
		sign = "-"
	}

	// find out where to put the dot '.'
	if len(s) <= sdk.Precision {
		s = fmt.Sprintf("%s0.%018s", sign, s)
	} else {
		s = fmt.Sprintf("%s%s.%s", sign, s[:len(s)-sdk.Precision], s[len(s)-sdk.Precision:])
	}

	d, err := sdk.NewDecFromStr(s)
	if err != nil {
		require.Fail(t, err.Error())
	}
	return d
}

// connect the test inputs to the actual code
func executeTest(t *testing.T, s TestInput) {
	arg1 := bigintToDec(t, &s.arg1.value)
	arg2 := bigintToDec(t, &s.arg2.value)
	switch s.opcode {
	case "newDecFromInt":
		if s.result.error {
			require.Panics(t, func() { sdk.NewDecFromInt(sdk.NewIntFromBigInt(&s.arg1.value)) })
		}

	case "add":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.Add(arg1, arg2) })
		} else {
			actual := sdk.Dec.Add(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "sub":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.Sub(arg1, arg2) })
		} else {
			actual := sdk.Dec.Sub(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "mul":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.Mul(arg1, arg2) })
		} else {
			actual := sdk.Dec.Mul(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "mulTruncate":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.MulTruncate(arg1, arg2) })
		} else {
			actual := sdk.Dec.MulTruncate(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "quo":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.Quo(arg1, arg2) })
		} else {
			actual := sdk.Dec.Quo(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "quoTruncate":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.QuoTruncate(arg1, arg2) })
		} else {
			actual := sdk.Dec.QuoTruncate(arg1, arg2)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "ceil":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.Ceil(arg1) })
		} else {
			actual := sdk.Dec.Ceil(arg1)
			expected := bigintToDec(t, &s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	case "roundInt":
		if s.result.error {
			require.Panics(t, func() { sdk.Dec.RoundInt(arg1) })
		} else {
			actual := sdk.Dec.RoundInt(arg1)
			expected := sdk.NewIntFromBigInt(&s.result.value)
			assert.Equal(t, expected, actual, "the results should be equal")
		}

	default:
		// ignore
	}
}

func ExecFromItf(t *testing.T, filename string) {
	var states = parseItf(filename)
	for _, s := range states {
		description :=
			fmt.Sprintf("%s_%s_%s", s.opcode, s.arg1.value.String(), s.arg2.value.String())
		t.Run(description, func(t *testing.T) {
			executeTest(t, s)
		})
	}
}

// the actual tests reading from the JSON files

// Just one randomly generated test
func TestOneRun(t *testing.T) {
	ExecFromItf(t, "../test-inputs-v0.46.4/oneRandom.itf.json")
}

// a slightly longer test of 56 operations
func Test56ops(t *testing.T) {
	ExecFromItf(t, "../test-inputs-v0.46.4/random56.itf.json")
}

// This test demonstrates how Dec.Ceil can drive us outside of
// the required bit length without producing a panic.
// This test was produced with `quint verify`:
//
//	quint verify --step=stepCeil --invariant=bitLenOkWhenNoErrorNoCtor \
//	  --out-itf=ceilBitLen.itf.json decimalTest.qnt
func TestCeil(t *testing.T) {
	ExecFromItf(t, "../test-inputs-v0.46.4/ceilBitLen.itf.json")
}

// This test demonstrates how addition and multiplication may panic
// even though the result could be represented as a decimal.
// This is caused by the test for MAX_DEC_BIT_LEN.
//
//	quint verify --max-steps=1 --step=stepAdd --invariant=noErrorWhenIsDec \
//	  --out-itf=addErrorOnBitlen.itf.json decimalTest.qnt
func TestAddErrorOnBitlen(t *testing.T) {
	ExecFromItf(t, "../test-inputs-v0.46.4/addErrorOnBitlen.itf.json")
	ExecFromItf(t, "../test-inputs-v0.46.4/mulErrorOnBitlen.itf.json")
}
