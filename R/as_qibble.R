#' @export
is.qibble <- function(x){
  inherits(x, 'qibble') 
}

#' @export
as.qibble <- function(x) {
  UseMethod('as.qibble')
}

#' @export
as.qibble.qibble <- function(x) {
  x
}

#' @export
as.qibble.tbl <- function(x) {
  class(x) <- c('qibble',class(x))
  x
}

#' @importFrom dplyr as.tbl
#' @export
print.qibble <- function(object){
  print(dplyr::as.tbl(object))
}

#' @inherit magrittr::`%>%`
#' @export
`%>%` <- magrittr::"%>%"