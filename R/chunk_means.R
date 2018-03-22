#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param chunk_time PARAM_DESCRIPTION
#' @param job_chunks PARAM_DESCRIPTION
#' @param chunk_complete PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname chunk_means
#' @export 
#' @importFrom tibble enframe
#' @import dplyr
#' @importFrom tidyr unnest
chunk_means <- function(chunk_time,job_chunks,chunk_complete){
  
  if(length(chunk_complete)==0) return(NULL)
  
  complete_vec <- tibble::enframe(chunk_complete)%>%dplyr::pull(value)
  
  if(length(job_chunks)!=length(complete_vec)) return(NULL)
  
  split(chunk_time,
        rep(job_chunks,complete_vec))%>%
    tibble::enframe()%>%
    tidyr::unnest()%>%
    dplyr::rename(chunk=name)%>%
    dplyr::group_by(chunk)%>%
    dplyr::summarise(chunk_mean_time=mean(value))%>%
    dplyr::ungroup() 
}
