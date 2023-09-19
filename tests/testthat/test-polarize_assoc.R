test_that("polarize_assoc returns a data frame", {
  data(toydata)
  
  # Apply the function to the toydata
  result <- polarize_assoc(data = toydata, value_1 = att_val, value_2 = party_ord, r_or_r2 = "r", by = c(att_name, group))
  
  # Check that the result is a data frame
  expect_true(is.data.frame(result))
})

