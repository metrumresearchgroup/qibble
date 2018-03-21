#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param workDir PARAM_DESCRIPTION, Default: 'qapply'
#' @param tag PARAM_DESCRIPTION, Default: 'stanrun'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_complete
#' @export 
get_complete <- function(
  workDir='qapply',
  tag='stanrun'){
  
    out <- gsub('outVec-|\\.Robj','',list.files(file.path(workDir,'out',tag)))
    
    out[order(as.numeric(sapply(strsplit(out,'-'),'[',1)))]
}
