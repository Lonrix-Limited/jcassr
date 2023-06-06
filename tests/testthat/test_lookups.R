

library(jcassr)

test_that("test: jc_do_multi_col_lookups", {

  data <- data.frame(
    mat = c("St", "Wd", "Zonk", "P", "P"),
    clim = c("D", "W", "M", "Zonk", "D")
  )

  lookups <- data.frame(
    lookup_name = c(rep("materials", 3), rep("climates", 3)),
    keys = c("St", "P", "Wd", "W", "D", "M"),
    values = c("Steel", "Plastic", "Wood", "Wet", "Dry", "Moderate")
  )

  columns <- data.frame(
    source_column = c("mat", "clim"),
    lookup_name = c("materials", "climates"),
    target_column = c("mat", "clim2")
  )

  tt <- jc_do_multi_col_lookups(data, columns, lookups)

  #Check first column values which should be REPLACED with lookup values
  expect_equal("Steel", tt[[1, "mat"]])
  expect_equal("Wood", tt[[2, "mat"]])
  expect_equal(NA_character_, tt[[3, "mat"]]) #Zonk not found
  expect_equal("Plastic", tt[[4, "mat"]])
  expect_equal("Plastic", tt[[5, "mat"]])


  #Check second column values which should be unchanged since we specified a
  # different target column
  expect_equal("D", tt[[1, "clim"]])
  expect_equal("W", tt[[2, "clim"]])
  expect_equal("M", tt[[3, "clim"]])
  expect_equal("Zonk", tt[[4, "clim"]])
  expect_equal("D", tt[[5, "clim"]])

  #Check newly added 'clim2' column values which should be the lookup values
  # when lookued up on 'clim'
  expect_equal("Dry", tt[[1, "clim2"]])
  expect_equal("Wet", tt[[2, "clim2"]])
  expect_equal("Moderate", tt[[3, "clim2"]])
  expect_equal(NA_character_, tt[[4, "clim2"]])
  expect_equal("Dry", tt[[5, "clim2"]])


})


test_that("test: jc_get_invalid_vals", {

  data <- data.frame(
    priority = c("low", "medium", "high", "med", "very high", "high"),
    climate = c("Dry", "Wet", "Moderate", "Boderate", "Dry", "Bet")
  )

  lookups <- data.frame(
    lookup_name = c(rep("priorities", 3), rep("climates", 3)),
    keys = c("low", "medium", "high", "Wet", "Moderate", "Dry")
  )

  columns <- data.frame(
    source_column = c("priority", "climate"),
    lookup_name = c("priorities", "climates")
  )

  tt <- jcassr::jc_get_invalid_vals(data, columns, lookups)

  #should be 4 invalid codes:
  expect_equal(4, nrow(tt))

  expect_equal("priority", tt[[1, "column_name"]])
  expect_equal("med", tt[[1, "invalid_values"]])

  expect_equal("very high", tt[[2, "invalid_values"]])
  expect_equal("priority", tt[[2, "column_name"]])


  expect_equal("Boderate", tt[[3, "invalid_values"]])
  expect_equal("climate", tt[[3, "column_name"]])

  expect_equal("Bet", tt[[4, "invalid_values"]])
  expect_equal("climate", tt[[4, "column_name"]])


})
