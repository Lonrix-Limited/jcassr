library(jcassr)

# test_that("Normalise_Percentiles", {
#
#   library(readxl)
#   infile <- "C:/Users/fritz/Juno Services Dropbox/Local_Authorities/Christchurch CC/2023/jcass/input_data_imputed.csv"
#   fwp_data <- read.csv(infile)
#
#   quants <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.98, 1)
#   scales <- c(0.0, 0.5, 1.0, 1.5, 2.00, 2.5, 3.00, 3.50, 4)
#
#   values <- fwp_data$pct_poth
#   browser()
#   norm_values <- jc_normalise_percentiles(values, quants, scales)
#   summary(norm_values)
#
#   expect_equal(round(norm_values[1],2), -1.3363)
#   expect_equal(round(norm_values[3],2), -0.2673)
#   expect_equal(round(norm_values[4],2), 0.2673)
#   expect_equal(round(norm_values[5],2), 0.8018)
#
#
#
#   #Test with explicitly provided mean and std deviation, and scalar
#   norm_value <- jc_normalise_z(2, mean_val = 5, stdev = 3.74166)
#   expect_equal(round(norm_value, 4), -0.8018)
#
# })


test_that("Present Worth Factor", {

  discount_perc <- 6.3
  inflation_perc <- 4.7
  period = 8
  discount_factor <- jc_get_PWF(period, inflation_perc, discount_perc)
  expect_equal(round(discount_factor,4), 0.8857)


  discount_perc <- 1.33
  inflation_perc <- 7.65
  period = 3
  discount_factor <- jc_get_PWF(period, inflation_perc, discount_perc)
  expect_equal(round(discount_factor,4), 1.1990)

})


test_that("Min-Max Normalise", {

  values <- c(0,2,4,6,8,10)
  norm_values <- jc_normalise_min_max(values)
  expect_equal(norm_values[1], 0)
  expect_equal(norm_values[3], 0.4)
  expect_equal(norm_values[5], 0.8)
  expect_equal(norm_values[6], 1)

  # Explicitly set min and max. With max now double the previous, the normalised
  # values should be half of the previous ones
  norm_values <- jc_normalise_min_max(values, min_val = 0, max_val = 20)
  expect_equal(norm_values[1], 0)
  expect_equal(norm_values[3], 0.2)
  expect_equal(norm_values[5], 0.4)
  expect_equal(norm_values[6], 0.5)

  values <- c(-10,-5,0,5,10)
  norm_values <- jc_normalise_min_max(values)
  expect_equal(norm_values[1], 0)
  expect_equal(norm_values[2], 0.25)
  expect_equal(norm_values[3], 0.5)
  expect_equal(norm_values[4], 0.75)
  expect_equal(norm_values[5], 1)

  values <- c(-10,-5,0,5,10)
  norm_values <- jc_normalise_min_max(values, min_val = -5, max_val = 5)
  expect_equal(norm_values[1], 0)
  expect_equal(norm_values[2], 0)
  expect_equal(norm_values[3], 0.5)
  expect_equal(norm_values[4], 1)
  expect_equal(norm_values[5], 1)

  #Test with scalar values - 65 lies halfway between 10 and 120. Should get 0.5
  norm_value <- jc_normalise_min_max(65, min_val = 10, max_val = 120)
  expect_equal(norm_value, 0.5)

})


test_that("Z-Value Normalise", {

  values <- c(0,2,4,6,8,10)
  norm_values <- jc_normalise_z(values)
  expect_equal(round(norm_values[1],4), -1.3363)
  expect_equal(round(norm_values[3],4), -0.2673)
  expect_equal(round(norm_values[4],4), 0.2673)
  expect_equal(round(norm_values[5],4), 0.8018)



  #Test with explicitly provided mean and std deviation, and scalar
  norm_value <- jc_normalise_z(2, mean_val = 5, stdev = 3.74166)
  expect_equal(round(norm_value, 4), -0.8018)

})

test_that("COST354_Index", {

  # Test of calculation of combined index using the COST354 report approach.
  # This test uses the example data on P61 of the Cost Report, with values
  # shuffled a bit
  index_values <- c(3.67, 1.01, 2.63, 3.95, 2.41)
  weights <- c(1.0, 0.6, 0.7, 0.4, 0.5 )

  cpi <- get_COST354_index(index_values, weights, 5, 20)  #3.93 (round,2)
  expect_equal(round(cpi,2), 3.93)

  # Test of calculation of combined index using the COST354 report approach.
  # This test uses the example data on P61 of the Cost Report, with values
  # shuffled a bit
  index_values <- c( 1.23, 4.38, 0.0, 0.24, 3.98 )
  weights <- c(0.9, 0.8, 0.5, 0.0, 0.66 )

  cpi <- get_COST354_index(index_values, weights, 5, 33)  #4.2356 (round,4)
  expect_equal(round(cpi,4), 4.2356)

})

test_that("get percent ranking", {

  # Check vs Excel
  values <- c(1, 10, 8, 4, 7, 3, 7)
  excel_ranks <- c(14.29, 100.00, 85.71, 42.86, 64.29, 28.57, 64.29)

  df <- data.frame(test = values)

  ranks <- jc_get_rankpercent(df, "test")

  for (i in  1:length(values)) {

    excel_val <- round(excel_ranks[i],2)
    jc_val <- round(ranks[i],2)

    expect_equal(jc_val, excel_val)

  }
})

test_that("get percent ranking - reverse", {

  # Check vs Excel
  values <- c(1, 10, 8, 4, 7, 3, 7)
  excel_ranks <- c(100.00, 14.29, 28.57, 71.43, 50.00, 85.71, 50.00)

  df <- data.frame(test = values)

  ranks <- jc_get_rankpercent(df, "test", reverse = TRUE)

  for (i in  1:length(values)) {

    excel_val <- round(excel_ranks[i],2)
    jc_val <- round(ranks[i],2)

    expect_equal(jc_val, excel_val)

  }

})





