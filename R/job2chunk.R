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
#' @rdname job2chunk
#' @export 
job2chunk <- function(x){
  gsub('^stanrun-|\\.err$',
       '',
       basename(gsub('^(.*?)NONE',
                     '',
                     system(sprintf('qstat -explain a -j %s  | grep ^stderr_path_list',x),intern = TRUE)
       )
       )
  ) 
}
