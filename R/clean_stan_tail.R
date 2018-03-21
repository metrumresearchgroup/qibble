#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clean_stan_tail
#' @export 
clean_stan_tail <- function(value){
  value <- gsub('^\\s+','',value)
  value <- gsub('\\s{2,}',' ',value)
  value <- gsub('\\[ ','[',value)
  value <- gsub('\\/ ','',value)
  value
}
