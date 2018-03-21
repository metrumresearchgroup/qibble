#' @import dplyr
#' @importFrom purrr map map_chr
#' @importFrom parallel mclapply detectCores
#' @importFrom utils tail
qapply_qibble <- function(d,workDir,tag){
  
  d <- d%>%
    dplyr::mutate(job_chunks = purrr::map(jobs,.f=function(y){
      
      unlist(parallel::mclapply(X = y,
                                FUN = job2chunk,
                                mc.cores = parallel::detectCores()-1))
    }))
  
  err_path <- file.path(workDir,'err',tag)
  err_files <- list.files(err_path,full.names = TRUE)
  
  log_path <- file.path(workDir,'log',tag)
  log_files <- list.files(log_path,full.names = TRUE)
  
  
  err_out <- lapply(err_files,function(x) paste0(readLines(x),collapse = '\n'))
  names(err_out) <- basename(err_files)
  
  log_out <- parallel::mclapply(log_files,function(x) paste0(readLines(x),collapse = '\n'),mc.cores = parallel::detectCores()-1)
  names(log_out) <- basename(log_files)
  
  d$job_err <- lapply(d$job_chunks,function(y) unlist(sapply(y,function(x) err_out[grep(sprintf('\\b%s\\b',x),names(err_out))],USE.NAMES = FALSE)))
  d$job_log <- lapply(d$job_chunks,function(y) unlist(sapply(y,function(x) log_out[grep(sprintf('\\b%s\\b',x),names(log_out))],USE.NAMES = FALSE)))
  
  d$job_log_tail <- purrr::map(d$job_log,function(y) purrr::map_chr(y,.f=function(x) utils::tail(strsplit(x,'\n')[[1]],1)))
  
  outvec <- list.files(file.path(workDir,'out',tag))
  
  d <- d%>%
    dplyr::mutate(job_complete=purrr::map(job_chunks,.f=function(y) unlist(sapply(y,function(x) outvec[grep(sprintf('\\b%s\\b',x),outvec)],USE.NAMES = FALSE))))
  
  d$slot_complete <- sapply(d$job_complete,length)
  
  d$chunk_time <- lapply(d$job_log,function(y) lapply(y,function(x) dplyr::data_frame(gt=get_time(x)))%>%
                           dplyr::bind_rows()%>%
                           unlist(use.names = FALSE))
  
  d$chunk_time_mean <- lapply(d$chunk_time,function(y){
    if(is.null(y)) return(NULL)
    if(all(is.na(y))) return(NULL)
    mean(y,na.rm=TRUE)
  })
  
  d$chunk_complete <- lapply(d$job_log,function(y) sapply(y,function(x){
    gx <- get_counter(x)
    ifelse(length(gx)==0,0,max(gx))
  }))
  
  # names ----  
  
  names(d$chunk_complete) <- d$ip
  names(d$chunk_time_mean) <- d$ip
  names(d$chunk_time) <- d$ip
  names(d$job_log) <- d$ip
  names(d$job_err) <- d$ip
  names(d$job_chunks) <- d$ip

  return(d)
}
