#' @title filter_scale_length
#' 
#' @description Helper function to filter ordered ratings scales with a minimum length of unique values.
#' 
#' @param data A data set object.
#' @param scale_names A column containing scale names.
#' @param scale_values A column containing scale values.
#' @param min_scale_length The threshold below which to remove ordered ratings scales.
#' 
#' @details
#' Many of the approaches to measuring polarization collected in this package are sensitive to or assume a certain scale length (see van der Eijk 2001). Hence, this function provides a convenient way to filter out variables that do not meet a user-specified threshold, which can be called prior to passing survey data to \code{\link{polarize_distr}} or \code{\link{polarize_assoc}}.
#' 
#' @references
#' 
#' Van Der Eijk, C. (2001). Measuring Agreement in Ordered Rating Scales. Quality and Quantity, 35:325–341.
#' 
#' @seealso [`polarize_distr()`][polarize_distr], [`polarize_assoc()`][polarize_assoc]
#' 
#' @return The data set with ordered ratings scales below the threshold removed.
#' 
 
filter_scale_length <- function(
    data,
    scale_names,
    scale_values,
    min_scale_length = 4
    ) {

  scale_length <- function(x, na.rm = TRUE){
    if(na.rm){
      x <- x[!is.na(x)]
    }
    (length(unique(x)))
  }

  meets_min_scale <- data |>
    summarise(
      scale_lengths = scale_length({{ scale_values }}),
      .by = {{ scale_names }}
      ) |>
    filter(scale_lengths >= min_scale_length) |>
    pull({{ scale_names }})

  output <- data %>%
    filter({{ scale_names }} %in% meets_min_scale)

  return(output)

}
