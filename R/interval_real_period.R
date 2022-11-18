#' Calculate the non overlapping actual medication days of patients in the intersection of multiple times.
#'
#' Calculate non overlapping time.
#'
#' At least three column names must be given, including "patient id", "prescription start time","prescription end time".
#'
#' @param data prescription patient table
#' @param patient_id a  "patient_id" column name or a similar column
#' @param st_dates a  "Prescription start time" column name or a similar column
#' @param end_dates a  "Prescription end time" column name or a similar column
#'
#'
#' @return
#'    Actual non overlapping medication days per patient
#'
#'
#' @export
#' @examples
#' \dontrun{
#' ## Build data
#' prec_data = data_med$prec %>%
#'   mutate(prec_end_date = as.Date(prec_date) + sample(1:30,1)) %>%
#'   rename(prec_start_date = prec_date)
#'
#' interval_real_period(prec_data,patient_id = "patient_id",
#'                      st_dates = "prec_start_date",
#'                      end_dates = "prec_end_date")
#' }


interval_real_period = function(data,patient_id = "patient_id",
                                st_dates = "prec_start_date",
                                end_dates = "prec_end_date"){
  if(is.null(data)){
    stop("input datatable is wrong,please recheck data")
  }else if(unique(c(patient_id,st_dates,end_dates) %in% names(data))){
    aim_data <- data %>%
      dplyr::select(patient_id,st_dates,end_dates)
    names(aim_data) = c("patient_id","st_dates","end_dates")
    aim_data %>%
      tr(.,c("st_dates","end_dates"),"dat") %>%
      distinct() %>%
      rowwise() %>%
      do(tibble(patient_id = .$patient_id,
                Date = seq(.$st_date,.$end_date,by=1))) %>%
      distinct() %>% ungroup() %>%
      count(patient_id) %>% rename(use_days = n)
  }else{
    return(message("Unknown error occurred in dataset. \nPlease recheck again"))}

}




