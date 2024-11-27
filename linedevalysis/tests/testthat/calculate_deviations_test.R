describe("calculate_deviations()", {
  it("rejects inputs that aren't data frames", {
    expect_error(calculate_deviations("test.gpx"))
    expect_error(calculate_deviations(c(1,2)))
  })
})