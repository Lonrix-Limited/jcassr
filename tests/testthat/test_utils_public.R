
library(jcassr)

test_that("get NA percentables", {

  col1 <- seq(1,10, by = 1)
  col2 <- col1
  col2[2] <- NA

  col3 <- col1
  col3[3] <- NA
  col3[4] <- NA
  col3[5] <- NA
  col3[10] <- NA

  col4 <- rep(NA, 10)

  df <- data.frame(column1 = col1, column2 = col2, column3 = col3,
                   column4 = col4)

  result <- jc_get_NA_percentages(df)

  expect_equal(result[[1, "na_count"]], 0)
  expect_equal(result[[1, "na_percent"]], 0)

  expect_equal(result[[2, "na_count"]], 1)
  expect_equal(result[[2, "na_percent"]], 10)

  expect_equal(result[[3, "na_count"]], 4)
  expect_equal(result[[3, "na_percent"]], 40)

  expect_equal(result[[4, "na_count"]], 10)
  expect_equal(result[[4, "na_percent"]], 100)

  cols <- c("column1", "column3", "column4")
  result <- jc_get_NA_percentages(df, cols)

  expect_equal(result[[1, "na_count"]], 0)
  expect_equal(result[[1, "na_percent"]], 0)

  expect_equal(result[[2, "na_count"]], 4)
  expect_equal(result[[2, "na_percent"]], 40)

  expect_equal(result[[3, "na_count"]], 10)
  expect_equal(result[[3, "na_percent"]], 100)


})

test_that("piecewise linear model - not extrapolating", {

  #seg 2 overlap on right side of seg 1
  expect_equal(3, jc_get_element_overlap(0,6,3,99))

  xvals <- c(4,8,10,12)
  yvals <- c(4,10,3, 0)

  expect_equal(7, jc_utils_get_plmvalue(6, xvals, yvals))  #7
  expect_equal(6.5, jc_utils_get_plmvalue(9, xvals, yvals)) #6.5
  expect_equal(3, jc_utils_get_plmvalue(10, xvals, yvals)) # 3
  expect_equal(1.5, jc_utils_get_plmvalue(11, xvals, yvals)) #1.5

  expect_equal(4, jc_utils_get_plmvalue(2, xvals, yvals))  #4
  expect_equal(4, jc_utils_get_plmvalue(0, xvals, yvals)) #4
  expect_equal(0, jc_utils_get_plmvalue(14, xvals, yvals)) # 0

})

test_that("piecewise linear model - extrapolating", {


  xvals <- c(4,8,10,12)
  yvals <- c(4,10,3, 0)

  expect_equal(7, jc_utils_get_plmvalue(6, xvals, yvals, TRUE))  #7
  expect_equal(6.5, jc_utils_get_plmvalue(9, xvals, yvals, TRUE)) #6.5
  expect_equal(3, jc_utils_get_plmvalue(10, xvals, yvals, TRUE)) # 3
  expect_equal(1.5, jc_utils_get_plmvalue(11, xvals, yvals, TRUE)) #1.5

  expect_equal(1, jc_utils_get_plmvalue(2, xvals, yvals, TRUE))  #1
  expect_equal(-2, jc_utils_get_plmvalue(0, xvals, yvals, TRUE)) #-2
  expect_equal(-3, jc_utils_get_plmvalue(14, xvals, yvals, TRUE)) # -3

})

test_that("jc_get_boolean", {

  expect_equal(jc_get_boolean(NA), FALSE)
  expect_equal(jc_get_boolean(""), FALSE)
  expect_equal(jc_get_boolean(0), FALSE)
  expect_equal(jc_get_boolean("NO"), FALSE)
  expect_equal(jc_get_boolean("N"), FALSE)
  expect_equal(jc_get_boolean("no"), FALSE)
  expect_equal(jc_get_boolean("n"), FALSE)
  expect_equal(jc_get_boolean("FALSE"), FALSE)
  expect_equal(jc_get_boolean("false"), FALSE)
  expect_equal(jc_get_boolean("f"), FALSE)
  expect_equal(jc_get_boolean("  "), FALSE)
  expect_equal(jc_get_boolean("zombie"), FALSE)

  expect_equal(jc_get_boolean(1), TRUE)
  expect_equal(jc_get_boolean("YES"), TRUE)
  expect_equal(jc_get_boolean("Y"), TRUE)
  expect_equal(jc_get_boolean("yes"), TRUE)
  expect_equal(jc_get_boolean("y"), TRUE)
  expect_equal(jc_get_boolean("TRUE"), TRUE)
  expect_equal(jc_get_boolean("true"), TRUE)


})

test_that(".jc_check_column_name_ok", {

  #TODO: Freaking not working! Fix
  # expect_equal(.jc_check_column_name_ok("dsgfsdfg", "test", FALSE), TRUE)
  # expect_equal(.jc_check_column_name_ok("my_name", "test", FALSE), TRUE)
  # expect_equal(.jc_check_column_name_ok("joe bloggs", "test", FALSE), FALSE)
  # expect_equal(.jc_check_column_name_ok("sally$ride", "test", FALSE), FALSE)
  # expect_equal(.jc_check_column_name_ok("sally@ride", "test", FALSE), FALSE)
  # expect_equal(.jc_check_column_name_ok("sally~ride", "test", FALSE), FALSE)


})
