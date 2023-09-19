
test_that("filter_scale_length filters out scales with fewer than 4 unique values", {
  data(toydata)
  
  # Apply the function to the toydata
  filtered_data <- filter_scale_length(toydata, scale_names = att_name, scale_values = att_val)
  
  # Check that all scales in the filtered data have at least 4 unique values
  scale_lengths <- filtered_data  |>
    group_by(att_name) |>
    summarise(scale_length = n_distinct(att_val, na.rm = TRUE))
  
  expect_true(all(scale_lengths$scale_length >= 4))
})