
library("jcassr")


test_that("moora_ranking", {

  # Calculations and unit testing based on this video:
  # https://www.youtube.com/watch?v=4ikr_4OltUw

  # data for each attribute (columns) and option (rows)
  data <- data.frame(
    speed=c(3.5, 3.1, 3.6, 3, 3.3, 3.6, 3.5),
    memory=c(6, 4, 6, 4, 6, 6, 6),
    storage=c(1256, 1000, 2000, 1000, 1008, 1000, 1256),
    graphics=c(4, 2, 4, 2, 4, 2, 2),
    ram=c(16, 8, 16, 8, 12, 16, 16),
    resol=c(3, 1, 3, 2, 3, 3, 1),
    screen=c(17.3, 15.6, 17.3, 17.3, 15.6, 15.6, 15.6),
    reliab=c(8, 5, 5, 5, 8, 5, 6),
    weight=c(2.82, 3.08, 2.9, 2.6, 2.3, 2.8, 2.9),
    cost=c(4100, 3800, 4000, 3500, 3800, 4000, 4000)
  )

  #one weight for each attribute
  weights <- c(0.297, 0.025, 0.035, 0.076, 0.154, 0.053, 0.104,
               0.017, 0.025, 0.214)

  obj_types <- c("max", "max", "max", "max", "max", "max", "max",
                 "max", "min", "min")

  result <- jc_moora_rank(data, weights, obj_types)

  expect_equal(result[[1, "moora_rank"]], 2)
  expect_equal(result[[2, "moora_rank"]], 7)
  expect_equal(result[[3, "moora_rank"]], 1)
  expect_equal(result[[4, "moora_rank"]], 6)
  expect_equal(result[[5, "moora_rank"]], 3)
  expect_equal(result[[6, "moora_rank"]], 4)
  expect_equal(result[[7, "moora_rank"]], 5)

  expect_equal(round(result[[1, "moora_value"]],3), 0.226)
  expect_equal(round(result[[2, "moora_value"]],3), 0.135)
  expect_equal(round(result[[3, "moora_value"]],3), 0.236)
  expect_equal(round(result[[4, "moora_value"]],3), 0.152)
  expect_equal(round(result[[5, "moora_value"]],3), 0.203)
  expect_equal(round(result[[6, "moora_value"]],3), 0.202)
  expect_equal(round(result[[7, "moora_value"]],3), 0.186)

})


