context("Check etracting functions")

test_that("Check extract_libraries()",{
  expect_equal(extract_libraries("das library(aaa)"), "aaa")
})

