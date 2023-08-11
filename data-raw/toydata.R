library(tibble)
library(tidyr)
library(dplyr)

set.seed(123)

n <- 2000

party_cats <- c("up", "down", "left", "right")
party_nums <- seq(1, 7, 1)

x <- tibble(
  group = sample(c("a", "b"), n, replace = TRUE),
  party_cat = sample(party_cats, n, replace = TRUE),
  party_ord = sample(party_nums, n, replace = TRUE),
  att2 = sample(c(0, 1), n, replace = TRUE),
  att3 = sample(c(1, 2, 3), n, replace = TRUE),
  att4 = sample(seq(1, 4, 1), n, replace = TRUE),
  att5 = sample(seq(1, 5, 1), n, replace = TRUE),
  att7 = sample(seq(1, 7, 1), n, replace = TRUE),
  att10 = sample(seq(1, 10, 1), n, replace = TRUE),
  att100 = sample(seq(1, 100, 1), n, replace = TRUE),
  att101 = sample(seq(1, 101, 1), n, replace = TRUE),
  time = "x",
  weight = runif(n, min = 0.1, max = 2),
  id = seq(1, n, 1)
)

y <- tibble(
  group = sample(c("a", "b"), n, replace = TRUE),
  party_cat = sample(party_cats, n, replace = TRUE),
  party_ord = sample(party_nums, n, replace = TRUE),
  att2 = sample(c(0, 1), n, replace = TRUE),
  att3 = sample(c(1, 2, 3), n, replace = TRUE),
  att4 = sample(seq(1, 4, 1), n, replace = TRUE),
  att5 = sample(seq(1, 5, 1), n, replace = TRUE),
  att7 = sample(seq(1, 7, 1), n, replace = TRUE),
  att10 = sample(seq(1, 10, 1), n, replace = TRUE),
  att100 = sample(seq(1, 100, 1), n, replace = TRUE),
  att101 = sample(seq(1, 101, 1), n, replace = TRUE),
  time = "y",
  weight = runif(n, min = 0.1, max = 2),
  id = seq(1, n, 1)
)

z <- tibble(
  group = sample(c("a", "b"), n, replace = TRUE),
  party_cat = sample(party_cats, n, replace = TRUE),
  party_ord = sample(party_nums, n, replace = TRUE),
  att2 = sample(c(0, 1), n, replace = TRUE),
  att3 = sample(c(1, 2, 3), n, replace = TRUE),
  att4 = sample(seq(1, 4, 1), n, replace = TRUE),
  att5 = sample(seq(1, 5, 1), n, replace = TRUE),
  att7 = sample(seq(1, 7, 1), n, replace = TRUE),
  att10 = sample(seq(1, 10, 1), n, replace = TRUE),
  att100 = sample(seq(1, 100, 1), n, replace = TRUE),
  att101 = sample(seq(1, 101, 1), n, replace = TRUE),
  time = "z",
  weight = runif(n, min = 0.1, max = 2),
  id = seq(1, n, 1)
)

toydata_l <- bind_rows(x, y, z) |>
  pivot_longer(
    cols = c(contains("att")),
    names_to = "att_name",
    values_to = "att_val"
  )

toydata_w <- toydata_l |>
  pivot_wider(
    names_from = att_name,
    values_from = att_val
  )

rm(n, party_cats, party_nums, x, y, z)

save(toydata_l, toydata_w, file = "data/toydata.RData")
usethis::use_data(toydata_l, toydata_w, overwrite = TRUE)

