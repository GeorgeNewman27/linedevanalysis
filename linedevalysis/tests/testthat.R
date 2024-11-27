# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
install.packages("/workspaces/137775831/CS50R/linedevalysis_1.0.0.tar.gz")
install.packages("geosphere")
library(linedevalysis)

test_check("linedevalysis")
