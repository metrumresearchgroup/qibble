#' @importFrom dplyr filter mutate count bind_rows
#' @importFrom purrr pluck map_df map
#' @importFrom ggplot2 ggplot aes geom_bar theme element_blank labs scale_x_continuous scale_y_discrete
#' @importFrom tidyr gather unnest
#' @importFrom ggridges geom_density_ridges
#' @import patchwork
#' @importFrom tibble as_tibble
#' @export
plot.qibble <- function(x,...){
  
  x <- tibble::as_tibble(x)
  
  ub <- (-1*eval(parse(text=x$job_chunks[[1]][1])))
  
  active <- get_nodes(tag = attr(x,'tag'))
  
  done   <- length(get_complete(workDir = attr(x,'workDir'),tag=attr(x,'tag')))
  
  total  <- active + done
  
  count_nodes <- x%>%
    dplyr::filter(slot_state=='run')%>%
    purrr::pluck('chunk_complete')%>%
    purrr::map_df(.f=function(x) data.frame(x)%>%
                    dplyr::mutate(chunk=rownames(.)),.id='ip')%>%
    dplyr::count(x)
  
  chunk_length <- -1*eval(parse(text=x$job_chunks[[1]][1])) + 1
  
  nodes_in_use <- x%>%dplyr::filter(slot_state=='run')%>%nrow()
  
  p1 <- x%>%
    dplyr::filter(slot_state=='run'&sapply(chunk_time_mean,length)>0)%>%
    ggplot2::ggplot(ggplot2::aes(x=ip,y=slots_used,fill=ip)) + 
    ggplot2::geom_bar(stat='identity',position='dodge',show.legend = FALSE) + 
    ggplot2::labs(y='Slots in Use',x='IP') +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
  
  p2_data <- purrr::map(x$chunk_complete[x$slot_state=='run'],
                        .f=function(x){
                          data.frame(complete=x)%>%
                            dplyr::mutate(chunk=rownames(.)) 
                        })%>%
    dplyr::bind_rows(.,.id='ip')%>%
    dplyr::mutate(complete=complete+1)%>%
    dplyr::count(ip,complete)
  
  max_y <- p2_data%>%
    dplyr::group_by(complete)%>%
    dplyr::summarise(nn=sum(n))%>%
    dplyr::ungroup()%>%
    dplyr::filter(nn==max(nn))%>%
    dplyr::pull(nn)
  
  p2 <- p2_data%>%
    ggplot2::ggplot(ggplot2::aes(x=complete,y=n,fill=ip)) + 
    ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
    ggplot2::labs(y='Frequency',x='Jobs Complete') + 
    #ggplot2::scale_y_continuous(breaks=0:max_y) +
    ggplot2::scale_x_continuous(limits=c(0,(chunk_length + 1)),breaks=1:chunk_length) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
  
  p3 <- x%>%
    dplyr::filter(slot_state=='run')%>%
    dplyr::select(ip,chunk_time)%>%
    tidyr::unnest()%>%
    ggplot2::ggplot(ggplot2::aes(x=chunk_time,y=ip,colour=ip, height = ..density..)) + 
    ggridges::geom_density_ridges(stat = "binline", bins = 20, scale = 0.95,show.legend = FALSE) +
    ggplot2::scale_y_discrete(expand = c(0.005, 0)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) + 
    ggplot2::labs(x='Time per Iteration',y='IP',
                  title=sprintf('Job Progress: %s/%s (%s Nodes in Use)',
                                done,total,nodes_in_use)
    )
  
  if( nrow(count_nodes)==1 ){
    
    p3 + p2
    
  } else {
    
    p3 + (p1 / p2)
    
  }  
}