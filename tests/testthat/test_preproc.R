
library(jcassr)

test_that("multiple column lookups", {

  data <- data.frame(
    val1 = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, NA, 7.7, 8.8, 9.9, NA, 10.1),
    val2 = c(100.1, 200.2, 300.3, 400.4, 500.5, 600.6, NA, 700.7, 800.8, 900.9,
             NA, 1000.1)
  )

  clamp_info <- data.frame(
    column_name = c("val1", "val2"),
    min_allowed = c(3, 150),
    max_allowed = c(10, 850)
  )

  tt <- jc_clamp_columns(data, clamp_info)


  expect_equal(3, tt[[1, "val1"]])      #clampled
  expect_equal(3, tt[[2, "val1"]])      #clampled
  expect_equal(3.3, tt[[3, "val1"]])    #original
  expect_equal(NA_real_, tt[[11, "val1"]])    #original
  expect_equal(10, tt[[12, "val1"]])    #clamped

  expect_equal(150, tt[[1, "val2"]])      #clampled
  expect_equal(200.2, tt[[2, "val2"]])    #original
  expect_equal(300.3, tt[[3, "val2"]])    #original
  expect_equal(NA_real_, tt[[11, "val2"]])    #original
  expect_equal(850, tt[[12, "val2"]])    #clamped

})


test_that("check required columns", {

  data <- data.frame(
    column_1 = c(runif(10)),
    column_2 = c(runif(10)),
    column_3 = c(runif(10))
  )

  req_cols <- c("column_1", "column_2", "Bongo")
  check <- jc_check_req_cols(data, req_cols) #should fail
  expect_equal(FALSE, check)

  req_cols <- c("column_1", "column_2", "column_3")
  check <- jc_check_req_cols(data, req_cols) #should pass
  expect_equal(TRUE, check)

})
