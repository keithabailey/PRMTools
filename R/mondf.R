#' Get the difference in months between two dates
#'
#' This function will retunr the number of months between two dates
#' @param d1 Date From
#' @param d2 Date To
#' @export
#' @examples
#' mondf()



#Get the difference in months between two dates
mondf <- function(d1, d2) {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(lubridate)

  monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon }
  monnb(d2) - monnb(d1)
}
