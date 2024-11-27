describe("read_line_gpx()", {
  it("rejects inputs that aren't strings or 2x2 matricies", {
    expect_error(read_line_gpx(1))
    expect_error(read_line_gpx(c("line1.gpx","line2.gpx")))
    expect_error(read_line_gpx())
  })
  it("accepts inputs that are strings and 2x2 matricies, but rejects invalid file names", {
    expect_error(read_line_gpx("test.gpx"))
    expect_output(read_line_gpx(rbind(c(40,50),c(30,40))))
  })
  it("accepts rejects matricies with the wrong dimensions or invlaid lat/long coordinates", {
    expect_error(read_line_gpx(rbind(c(181,50),c(30,-200))))
    expect_error(read_line_gpx(rbind(c(40,50),c(30,40),c(20,30))))
    expect_error(read_line_gpx(rbind(50,40,30)))
  })
})