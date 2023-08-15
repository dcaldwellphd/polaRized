library(tibble)

set.seed(123)

n <- 2000

toydata_w <- tibble(
  group = sample(c("them", "us"), n, replace = TRUE),
  party_cat = sample(c("up", "down", "left", "right", NA), n, replace = TRUE),
  party_ord = sample(c(seq(1, 7, 1), NA), n, replace = TRUE),
  att2val = sample(c(0, 1, NA), n, replace = TRUE),
  att4val = sample(c(seq(1, 4, 1), NA), n, replace = TRUE),
  att5val = sample(c(seq(1, 5, 1), NA), n, replace = TRUE),
  att10val = sample(c(seq(1, 10, 1), NA), n, replace = TRUE),
  att11val = sample(c(seq(0, 10, 1), NA), n, replace = TRUE),
  att100val = sample(c(seq(1, 100, 1), NA), n, replace = TRUE),
  att101val = sample(c(seq(0, 100, 1), NA), n, replace = TRUE),
  weight = runif(n, min = 0.1, max = 2),
  id = seq(1, n, 1)
)

toydata_l <- toydata_w |>
  tidyr::pivot_longer(
    cols = c(contains("att")),
    names_to = "att_name",
    values_to = "att_val"
  )

rm(n)

save(toydata_l, toydata_w, file = "data/toydata.RData")
usethis::use_data(toydata_l, toydata_w, overwrite = TRUE)