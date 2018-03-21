#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_time
#' @export 
get_time <- function(x){
  as.numeric(gsub('^(.*?)TIME: | #(.*?)$','',grep('### QAPPLY ####\\"',strsplit(x,'\n')[[1]],value=TRUE))) 
}
