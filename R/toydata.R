#' @importFrom tibble tibble
   NULL

#' Survey data on attitudes and partisanship
#'
#' Artificially generated survey data on attitudes and partisanship. It comes in two formats to illustrate how to use different functions from the package. The longer \code{toydata_l} records attitudes towards different issues using name-value keys. The wider \code{toydata_w} contains separate columns for attitude items. In both cases, rows record attitudes towards an issue among individuals organized by partisanship and group.
#'
#' @docType data
#'
#' @usage data(toydata)
#' 
#' @format Two objects of class \code{data.frame}
#' \describe{
#'  \item{toydata_l}{A data frame with 14,000 rows and 7 variables: \code{id}, \code{group}, \code{party_cat}, \code{party_ord}, \code{weight}, \code{att_name}, \code{att_val}}.
#' \item{toydata_w}{A data frame with 2000 rows and 12 variables, which are the same as \code{toydata_l} except that \code{att_name} and \code{att_val} are spread across 7 columns: \code{att2val}, \code{att4val}, \code{att5val}, \code{att10val}, \code{att11val}, \code{att100val}, \code{att101val}}.
#' }
#' @references These data sets were artificially generated for the polaRized package.
#' @keywords datasets
#' @examples
#' data(toydata)
#' head(toydata_l)
#' head(toydata_w) 
"toydata_l"
"toydata_w"