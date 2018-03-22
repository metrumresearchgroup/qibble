#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname current_jobs
#' @export 
#' @import dplyr
#' @importFrom tidyr unnest
current_jobs <- function(d){
  d%>%
    dplyr::filter(slot_state=='run')%>%
    dplyr::select(ip,chunk_name=job_chunks)%>%
    tidyr::unnest()%>%
    dplyr::group_by(ip)%>%
    dplyr::mutate(chunk_idx=1:n())%>%
    dplyr::left_join(
      d%>%
        dplyr::filter(slot_state=='run')%>%
        dplyr::select(ip,jobs_complete=chunk_complete)%>%
        tidyr::unnest()%>%
        dplyr::group_by(ip)%>%
        dplyr::mutate(chunk_idx=1:n()), by = c("ip", "chunk_idx")
    )%>%
    dplyr::left_join(
      d%>%
        dplyr::filter(slot_state=='run')%>%
        dplyr::select(ip,jobs)%>%
        tidyr::unnest()%>%
        dplyr::group_by(ip)%>%
        dplyr::rename(current_job=jobs)%>%
        dplyr::mutate(chunk_idx=1:n()), by = c("ip", "chunk_idx")
    )%>%
    dplyr::left_join(
      purrr::pmap(list(chunk_time=d$chunk_time,
                       job_chunks=d$job_chunks,
                       chunk_complete=d$chunk_complete),
                  .f=chunk_means)%>%
        bind_rows(.,.id='ip')%>%
        dplyr::rename(chunk_name=chunk), by = c("ip", "chunk_name")
    )%>%
    dplyr::select(-chunk_idx)%>%
    dplyr::ungroup()
}