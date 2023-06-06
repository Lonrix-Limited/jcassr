library(jcassr)

test_that("overlap with integers", {

  #seg 2 overlap on right side of seg 1
  expect_equal(3, jc_get_element_overlap(0,6,3,99))

  #seg 2 overlap on right side of seg 1
  expect_equal(1, jc_get_element_overlap(0,6,5,99))

  #seg 2 overlap on left side of seg 1
  expect_equal(3, jc_get_element_overlap(6,12,3,9))

  #seg 2 inside seg 1
  expect_equal(4, jc_get_element_overlap(1,10,1,5))

  #seg 1 inside seg 2
  expect_equal(5, jc_get_element_overlap(5,10,0,15))

  #no overlap
  expect_equal(0, jc_get_element_overlap(5,10,0,5))

  #no overlap
  expect_equal(0, jc_get_element_overlap(5,10,11,55))

})
