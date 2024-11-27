describe("read_route_gpx()", {
  it("rejects inputs that aren't vectors", {
    expect_error(read_route_gpx("test.gpx"))
    expect_error(read_route_gpx())
  })
  it("accepts inputs that are vectors, but rejects invalid file names", {
    expect_error(read_route_gpx(c("test.gpx")))
    expect_error(read_route_gpx(c("test1.gpx","test2.gpx")))
  })
})