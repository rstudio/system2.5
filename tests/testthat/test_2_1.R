describe("2.1.1. When pending, a promise", {
  it("2.1.1.1. may transition to either the fulfilled or rejected state", {
    p1 <- Promise$new()
    expect_identical(p1$status(), "pending")
    p1$resolve(NULL)
    expect_identical(p1$status(), "fulfilled")

    p2 <- Promise$new()
    expect_identical(p2$status(), "pending")
    p2$reject(NULL)
    expect_identical(p2$status(), "rejected")
  })
})

describe("2.1.2. When fulfilled, a promise", {
  p <- Promise$new()
  p$resolve(1)

  it("2.1.2.1. must not transition to any other state", {
    expect_identical(p$status(), "fulfilled")
    p$reject("failure")
    expect_identical(p$status(), "fulfilled")
  })

  it("2.1.2.2. must have a value, which must not change", {
    expect_identical(p$.__enclos_env__$private$value, 1)
    p$resolve(2)
    expect_identical(p$.__enclos_env__$private$value, 1)
  })
})

describe("2.1.3. When rejected, a promise", {
  p <- Promise$new()
  p$reject("error")

  it("2.1.3.1. must not transition to any other state", {
    expect_identical(p$status(), "rejected")
    p$resolve(1)
    expect_identical(p$status(), "rejected")
  })

  it("2.1.3.2. must have a reason, which must not change", {
    expect_identical(p$.__enclos_env__$private$value, simpleError("error"))
    p$reject("nope")
    expect_identical(p$.__enclos_env__$private$value, simpleError("error"))
  })
})
