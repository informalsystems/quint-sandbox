/**
 * A few tests to make sure that we do not break challenge2.
 */
module challenge2Test {
  import challenge2.* from "./challenge2"

  run senderTest = {
    val s0 = {
      allowance: tuples(ADDR, ADDR).mapBy((h, s) => 100),
      redeemed: tuples(ADDR, ADDR).mapBy((h, s) => 0)
    }
    val result = withdraw(s0, "Alice", 200, "Bob", "Alice")
    and {
      assert(result.error == ""),
      assert(result.output.redeemed.get(("Alice", "Bob")) == 200)
    }
  }

  run nonSenderTest = {
    val s0 = {
      allowance: tuples(ADDR, ADDR).mapBy((h, s) => 100),
      redeemed: tuples(ADDR, ADDR).mapBy((h, s) => 0)
    }
    val result = withdraw(s0, "Alice", 60, "Bob", "Charlie")
    and {
      assert(result.error == ""),
      assert(result.output.redeemed.get(("Charlie", "Bob")) == 60),
      assert(result.output.allowance.get(("Charlie", "Alice")) == 40)
    }
  }

  run nonSenderTooMuchTest = {
    val s0 = {
      allowance: tuples(ADDR, ADDR).mapBy((h, s) => 100),
      redeemed: tuples(ADDR, ADDR).mapBy((h, s) => 0)
    }
    val result = withdraw(s0, "Alice", 200, "Bob", "Charlie")
    and {
      assert(result.error == "not approved"),
    }
  }
}