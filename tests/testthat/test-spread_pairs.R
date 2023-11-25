if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
}

# Test that the correct number of attitude pairs are present in output
test_that("spread_pairs returns the correct number of attitude pairs", {
    data(toydata)
    
    result <- toydata |>
        spread_pairs(
            name_key = att_name, 
            value_key = att_val, 
            other_keys = id
        )
    
    expect_equal(
        pull(summarize(result, n = n_distinct(att_name1, att_name2))), 
        ncol(combn(unique(toydata$att_name), 2))
    )
})

# Test that the resulting name keys are not the same in any row
test_that("spread_pairs returns name key pairs comprising different attitude items", {
    data(toydata)
    
    result <- toydata |>
        spread_pairs(
            name_key = att_name, 
            value_key = att_val, 
            other_keys = id
        )
    
    expect_true(all(result$att_name1 != result$att_name2))
})

# Test that the resulting name keys are unique
# (i.e., name1-name2 and name2-name1 are not both present)
test_that("spread_pairs returns unique name key pairs", {
    data(toydata)
    
    result <- toydata |>
        spread_pairs(
            name_key = att_name, 
            value_key = att_val, 
            other_keys = id
        ) |>
        mutate(pairs = paste0(att_name1, "_", att_name2))
    
    reversed_pairs <- result |>
        mutate(rev_order = paste0(att_name2, "_", att_name1)) |>
        distinct(rev_order) |>
        pull(rev_order)
    
    expect_false(all(result$pairs %in% reversed_pairs))
})