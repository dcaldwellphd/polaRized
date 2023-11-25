if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
}

# Test function with no grouping variable
test_that("polarize_distr returns a single value with no grouping variables", {
    data(toydata)
  
    result <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std",
        by = NULL
    )
    
    expect_equal(nrow(result), 1)
})

# Test function with one grouping variable
test_that("polarize_distr returns one value for each level of a single grouping variable", {
    data(toydata)

    result <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std",
        by = att_name
    )

    expect_equal(nrow(result), dplyr::n_distinct(toydata$att_name))
})

# Test function with two grouping variables
test_that("polarize_distr returns one value for each level of multiple grouping variables", {
    data(toydata)

    result <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std",
        by = c(att_name, group)
    )

    expect_equal(
        nrow(result), 
        dplyr::n_distinct(toydata$att_name) * dplyr::n_distinct(toydata$group)
    )
})

# Test error stopping function when an unrecognized value is passed to the `measure` argument 
test_that("polarize_distr stops with error when measure is not one of the featured arguments", {
    data(toydata)

    expect_error(
        polarize_distr(
            toydata, 
            value = att_val, 
            measure = "foo"
        )
    )
})

# Test that function allows incorporation of survey design features
test_that("polarize_distr does not stop when supplied with a valid survey design feature", {
    data(toydata)

    expect_no_error(
        polarize_distr(
            toydata, 
            value = att_val, 
            measure = "std",
            weights = weight
        )
    )
})

# Test that survey design features are being incorparated
test_that("polarize_distr produces different values when survey design features are specified", {
    data(toydata)

    result1 <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std",
        weights = weight
    )

    result2 <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std"
    )

    expect_false(result1 == result2)
})

# Test normalization feature of the function
test_that("polarize_distr normalizes values if `rescale_0_1` argument is TRUE", {
    data(toydata)

    result <- polarize_distr(
        toydata, 
        value = att_val, 
        measure = "std",
        by = att_name,
        rescale_0_1 = TRUE
    )

    expect_true(min(result$att_val_std) >= 0)
    expect_true(max(result$att_val_std) <= 1)
})
