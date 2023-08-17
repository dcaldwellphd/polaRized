#' @importFrom tibble tibble
   NULL

#' Survey data on attitudes and partisanship
#'
#' Artificially generated survey data on attitudes and partisanship. Rows contains responses on ordered ratings scales  among individuals that are organized by attitude item, partisanship, and group.
#'
#' @docType data
#'
#' @usage data(toydata)
#'
#' @format An object of class \code{data.frame}
#' \describe{
#'  \item{id}{A serial ID column}
#'  \item{group}{Groups called "them" and "us"}
#'  \item{party_cat}{Unordered party categories called "left", "right", "liberal", and "conservative"}
#'  \item{party_ord}{Ordinal party categories between 1 and 7}
#'  \item{att_name}{The name of ordered ratings scales for items of different lengths commonly found in attitudinal research: `att2val`, `att4val`, `att5val`, `att10val`, `att11val`, `att100val`, `att101val`}
#'  \item{att_val}{Values across attitude items in `att_name`, ranging between 0 and 100}
#'  \item{weight}{Randomly generated sampling weights between 0.1 and 2}
#'  }
#'
#'
#' @references This data set was artificially generated for the polaRized package.
#' @keywords datasets
#' @examples
#' data(toydata)
#' head(toydata)
"toydata"
