library(eulerr)

png(
  file = "C:/Users/dtcal/Dropbox/ds-projects/polaRized-logo-image.png",
  height = 5, width = 5, units = "cm", res = 1200, bg = "#C3B1E1"
)

euler(
  c(
    "left" = 1000, 
    "right" = 1000,
    "left&right" = 400
  )
) |> 
  plot(
    quantities = NULL,
    edges = list(lex = 2),
    fills = c("#E34234", "#4682B4", "#C3B1E1"),
    labels = NULL
  )

dev.off()
