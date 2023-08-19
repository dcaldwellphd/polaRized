

library(dplyr)
library(tidyselect)
library(tidyr)
library(srvyr)
library(purrr)
library(survey)

data(toydata)

aa <- toydata |>
  filter(!(att_name %in% c("att100val", "att101val"))) |>
  polarize_assoc(
    value_1 = att_val,
    value_2 = party_ord,
    r_or_r2 = "r",
    by = c("att_name", "group"),
    weights = weight)





toydata |>
  filter_scale_length(
    scale_names = att_name,
    scale_values = att_val
    ) |>
  filter(!(att_name %in% c("att100val", "att101val"))) |>
  polarize_distr(
  value = att_val,
  measure = "Reardon",
  by = c("att_name", "group"),
  rescale_0_1 = TRUE,
  weights = weight
)




