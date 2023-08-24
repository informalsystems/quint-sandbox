// -*- mode: Bluespec; -*-
module kettleTest {
  import kettle.* from "./kettle"

  // the failover should work
  run failoverTest = {
    init
      .then(pressButton)
      .then(heat)
      .then(79.reps(_ => heat))
      .then(failover)
  }

  // depressing a button should work
  run depressTest = {
    init
      .then(pressButton)
      .then(heat)
      .then(depressButton)
  }

  // the kettle should not be able to heat up by 10000C
  run heatTooMuchTest = {
    init
      .then(pressButton)
      .then(10000.reps(_ => heat))
      .fail()
  }

  // this test fails. Why?
  run depressManyTestIgnore = {
    init
      .then(80.reps(_ => step))
      .then(depressButton)
  }

  temporal noOverheat = always(temperature <= 100)

  temporal eventuallyOff = eventually(not(heatingOn))

  temporal onThenOff = always(heatingOn implies eventually(not(heatingOn)))
}