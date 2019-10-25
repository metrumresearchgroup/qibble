#' @title qibble class
#' @description qibble class functions
#' @param x object to convert/check
#' @rdname qibble_class
#' @export
is.qibble <- function(x){
  inherits(x, 'qibble') 
}

#' @rdname qibble_class
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

#' @export
print.qibble <- function(x,...){
  class(x) <- class(x)[-1]
  print(x)
}

#' @inherit magrittr::`%>%`
#' @export
`%>%` <- magrittr::"%>%"