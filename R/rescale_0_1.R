rescale_0_1 <- function(x) {
  x_scaled <- x - min(x, na.rm = TRUE)
  x_scaled / max(x_scaled, na.rm = TRUE)
}
