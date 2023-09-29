if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Test function with no grouping variable
test_that("polarize_assoc returns a single value with no grouping variables", {
    data(toydata)
  
    result <- polarize_assoc(
        toydata, value_1 = att_val, 
        value_2 = party_ord, 
        r_or_r2 = "r",
        by = NULL
        )
    
    expect_equal(nrow(result), 1)
    
    })

# Test function with one grouping variable
test_that("polarize_assoc returns one value for each level of a single grouping variable", {
    data(toydata)

    result <- polarize_assoc(
        toydata, value_1 = att_val, 
        value_2 = party_ord, 
        r_or_r2 = "r",
        by = att_name
        )

    expect_equal(nrow(result), dplyr::n_distinct(toydata$att_name))
    
    })

# Test function with two grouping variables
test_that("polarize_assoc returns one value for each level of multiple grouping variables", {
    data(toydata)

    result <- polarize_assoc(
        toydata, value_1 = att_val, 
        value_2 = party_ord, 
        r_or_r2 = "r",
        by = c(att_name, group)
        )

    expect_equal(
        nrow(result), 
        dplyr::n_distinct(toydata$att_name) * dplyr::n_distinct(toydata$group)
        )
    })

# Test error stopping function when r_or_r2 is not "r" or "r2"
test_that("polarize_assoc stops with error when r_or_r2 is not 'r' or 'r2'", {
    data(toydata)

    expect_error(
        polarize_assoc(
            toydata, 
            value_1 = att_val, 
            value_2 = party_ord,
            r_or_r2 = "r3"
            )
        )
    })

# Test error stopping function when trying to calculate Pearson correlation with non-numeric values
test_that("polarize_assoc stops with error when r_or_r2 is 'r' and either value_1 or value_2 is non-numeric", {
    data(toydata)

    expect_error(
        polarize_assoc(
            toydata, 
            value_1 = att_val, 
            value_2 = party_cat,
            r_or_r2 = "r"
            )
        )
    })

# Test error stopping function when trying to calculate R-squared with a non-numeric value_1
test_that("polarize_assoc stops with error when r_or_r2 is 'r2' and value_1 is non-numeric", {
    data(toydata)

    expect_error(
        polarize_assoc(
            toydata, 
            value_1 = party_cat, 
            value_2 = att_val,
            r_or_r2 = "r2"
            )
        )
    })

# Test that function allows incorporation of survey design features
test_that("polarize_assoc does not stop when supplied with a valid survey design feature", {
    data(toydata)

    expect_no_error(
        polarize_assoc(
            toydata, 
            value_1 = att_val, 
            value_2 = party_ord,
            r_or_r2 = "r",
            weights = weight
            )
        )
    })

# Test that survey design features are being incorparated
test_that("polarize_assoc produces different values when survey design features are specified", {
    data(toydata)

    result1 <- polarize_assoc(
        toydata, 
        value_1 = att_val, 
        value_2 = party_ord,
        r_or_r2 = "r",
        weights = weight
        )

    result2 <- polarize_assoc(
        toydata, 
        value_1 = att_val, 
        value_2 = party_ord,
        r_or_r2 = "r"
        )

    expect_not_equal(result, result2)
    })