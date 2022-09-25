#' Count the number of patients in the table.
#'
#' count patient_id;
#'
#' need to change the patient id column in the table to "paitent_id"
#'
#' @param data a table contian a  "patient_id" column name or a similar column
#' @param raw_patient_id raw patient_id ,use for count ;Non-standard input
#'
#'
#' @return
#'    Total number of people after duplicate removal
#'
#'
#' @export
#' @examples
#' datatable1 = data.frame(
#' patient_id = paste0("patent",1:100),
#' visit_id  = paste0("visit",1:100))
#'
#' datatable2 = data.frame(
#'  patient = paste0("patent",1:100),
#'  visit_id  = paste0("visit",1:100))
#'
#' pe(datatable1)
#'
#' pe(datatable2,patient)


pe = function(data,raw_patient_id){
  raw_patient = enquo(raw_patient_id)
  raw_patient_cha = deparse(substitute(raw_patient_id))
  if (is.null(data)) {
    stop("input datatable is wrong,please recheck data")
  }else if(!"patient_id" %in% colnames(data) & is.null(raw_patient_cha)){
    stop("Please recheck colnames,
         or add a new column name that specifies the patient iD")
  }else if(!"patient_id" %in% colnames(data) &
           !is.null(raw_patient_cha) &
           raw_patient_cha %in% colnames(data)){
    return(data %>% select(!!raw_patient) %>% dplyr::n_distinct(.))
  }else if("patient_id" %in% colnames(data)){
    return(data %>% select(patient_id) %>% dplyr::n_distinct(.))
  }else{
    return(message("Unknown error occurred in dataset. \nPlease recheck again"))
  }}


