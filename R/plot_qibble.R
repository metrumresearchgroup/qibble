#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom tidyr gather
plot_qibble <- function(d){
  p1 <- d%>%
    dplyr::filter(slot_state=='run')%>%
    ggplot2::ggplot(ggplot2::aes(x=ip,y=slots_used,fill=ip)) + 
    ggplot2::geom_bar(stat='identity',position='dodge',show.legend = FALSE) + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::labs(y='Slots in Use',x='IP')
  
  ub <- (-1*eval(parse(text=d$job_chunks[[1]][1])))
  
  p2 <- purrr::map(d$chunk_complete[d$slot_state=='run'],.f=function(x) data.frame(complete=x)%>%dplyr::mutate(chunk=rownames(.)))%>%
    dplyr::bind_rows(.,.id='ip')%>%
    dplyr::mutate(complete=complete+1)%>%
    dplyr::count(ip,complete)%>%
    ggplot2::ggplot(ggplot2::aes(x=complete,y=n,fill=ip)) + 
    ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
    ggplot2::labs(y='Frequency',x='Jobs Complete') + 
    ggplot2::scale_x_continuous(limits=c(0,21),breaks=1:20)
  #ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  active <- get_nodes(tag = tag)
  done   <- length(get_complete(workDir = workDir,tag=tag))
  total  <- active + done
  
  p3 <- purrr::map(d$chunk_time,.f=function(x) data.frame(time=x)%>%
                     dplyr::mutate(chunk=rownames(.))%>%
                     tidyr::gather('time_id','time',-chunk))%>%
    dplyr::bind_rows(.,.id='ip')%>%
    dplyr::filter(!is.na(time))%>%
    dplyr::mutate(chunk=gsub('^stanrun-|\\.out$','',chunk))%>%
    ggplot2::ggplot(ggplot2::aes(x=time,y=ip,colour=ip)) + 
    ggridges::geom_ridges(rel_min_height = 0.1,alpha=0,show.legend = FALSE) +
    ggplot2::scale_y_discrete(expand = c(0.01, 0)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) + 
    ggplot2::labs(x='Time per Iteration',y='IP',
                  title=sprintf('Job Progress: %s/%s (%s Nodes in Use)',
                                done,
                                total,
                                d%>%
                                  dplyr::filter(slot_state=='run')%>%
                                  nrow()
                                )
                  )
  
  if( nrow(count_nodes)==1 ){
    
    p3 + p2
    
  } else {
    
    p3 + (p1 / p2)
    
  }  
}
