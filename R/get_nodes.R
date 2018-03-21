#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tag PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[xml2]{read_xml}},\code{\link[xml2]{xml_child}},\code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_text}},\code{\link[xml2]{xml_double}}
#' @rdname get_nodes
#' @export 
#' @import dplyr
#' @importFrom xml2 read_xml xml_child xml_find_all xml_text xml_double
get_nodes <- function(tag){
  tf <- paste0(qstat('-f -xml'),collapse = '\n')
  
  this_xml <- xml2::read_xml(tf)%>%
    xml2::xml_child('queue_info')%>%
    xml2::xml_find_all('Queue-List')
  
  this_xml_qw <- xml2::read_xml(tf)%>%
    xml2::xml_child('job_info')
  
  qw_num <- this_xml_qw%>%
    xml2::xml_find_all('job_list/state')%>%
    xml2::xml_text()%>%
    length()
  
  nodes_num <- qw_num
  
  slots <- 
    dplyr::data_frame(
      slots_used  = this_xml%>%
        xml2::xml_child(search = 'slots_used')%>%
        xml2::xml_double(),
               
      slot_state = this_xml%>%
        xml2::xml_child(search = 'state')%>%
        xml2::xml_text()
      )
  
  slots <- slots[slots$slots_used>0,]
  
  if(nrow(slots)>0){
    
    jobs <- this_xml%>%
      xml2::xml_find_all('job_list/JB_name')%>%
      xml2::xml_text()%>%
      split(.,rep(1:nrow(slots),slots$slots_used))
    
    jobs_num <- sapply(jobs,function(x) sum(x==tag),simplify = TRUE)
    
    nodes_num <- nodes_num + sum(jobs_num[which(is.na(slots$slot_state))])  
  }
  
  return(nodes_num)
}
