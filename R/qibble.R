#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param workDir PARAM_DESCRIPTION, Default: NULL
#' @param tag PARAM_DESCRIPTION, Default: NULL
#' @param qapply_log_tail PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_qibble
#' @export 
#' @import dplyr
#' @importFrom tidyr separate
#' @importFrom xml2 read_xml xml_child xml_find_all xml_double xml_text
qibble <- function(workDir=NULL,tag=NULL,qapply_log_tail = 1){
  
  this_xml <- xml2::read_xml(paste0(system('qstat -f -xml',intern = TRUE),collapse = '\n'))%>%
    xml2::xml_child('queue_info')%>%
    xml2::xml_find_all('Queue-List')
  
  d <- data_frame(
    name        = this_xml%>%xml2::xml_child(search = 'name')%>%xml2::xml_text(),
    slots_used  = this_xml%>%xml2::xml_child(search = 'slots_used')%>%xml2::xml_double(),
    slots_total = this_xml%>%xml2::xml_child(search = 'slots_total')%>%xml2::xml_double(),
    slot_state = this_xml%>%xml2::xml_child(search = 'state')%>%xml2::xml_text()
  )%>%
    dplyr::filter(slots_used>0)%>%
    tidyr::separate(name,c('group','node'),sep='@')%>%
    dplyr::mutate(
      ip = gsub('^ip-|\\.(.*?)$','',node),
      slot_state = ifelse(is.na(slot_state),'run',slot_state),
      slot_ratio = slots_used/slots_total,
      slot_ratio_char = ifelse(slot_ratio>1,slot_state,sprintf('%s/%s',slots_used,slots_total)),
      slot_ratio = ifelse(slot_ratio>1,0,slot_ratio),
      jobs = this_xml%>%xml2::xml_find_all('job_list/JB_job_number')%>%xml2::xml_double()%>%split(.,rep(1:n(),slots_used)),
      job_name = this_xml%>%xml2::xml_find_all('job_list/JB_name')%>%xml2::xml_text()%>%split(.,rep(1:n(),slots_used)),
      job_state = this_xml%>%xml2::xml_find_all('job_list/state')%>%xml2::xml_text()%>%split(.,rep(1:n(),slots_used))
    )

  names(d$job_state) <- d$ip
  names(d$job_name) <- d$ip  

  if('qapply'%in%rownames(installed.packages()))
     d <- qapply_qibble(d,workDir,tag,log_tail = qapply_log_tail)
  
  attr(d,which = 'workDir') <- workDir
  attr(d,which = 'tag') <- tag
  attr(d,which = 'class') <- c('qibble',class(d))
  
  return(d)
}
