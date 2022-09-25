#' Calculate `mean` and `sd` of a given variable
#'
#' @param data a dataframe
#' @param aim_var user-specified variables, using non-standard input.
#' @param ifunion default `FALSE`.when `ifunite = TRUE`，return a character
#'  contain `mean(sd)`.
#' @param digits indicating the number of decimal places (round) or significant digits (signif)
#' to be used. For round, negative values are allowed.
#' @return
#'     `mean_sd` return a data.frame which include two colunms.if `ifunion = true`
#'     retunr a character ,sucn as "mean(sd)".
#'
#' @export
#' @examples
#'\dontrun{
#' data(data_med)
#' pat_table = data_med$pat %>% calcu_age(.,birthdate)
#' mead_sd(pat_table,age)
#' pat %>% mean_sd(age)
#' pat %>% mean_sd(age,ifunion =TRUE)
#'}
#'

mean_sd = function(data,aim_var,ifunion = FALSE,digits  = 2){
 out =  data %>%
    select({{aim_var}}) %>%
    summarise(mean = mean({{aim_var}}),
              sd = sd({{aim_var}})) %>%
    mutate(across(where(is.numeric),~ round(.,digits)))

 if(ifunion == FALSE){
   out
 }else(
   return(paste0(out$mean,"(",out$sd,")"))
 )}


#' Calculate the number and proportion of patients
#'
#' @param numerator_data a dataframe subset
#' @param denominator_data a complete set of data, or a number used to represent
#          the total number of people.
#' @param patient_id This function uses `patient_ id` by default to calculate
#' the number of patients. If the column name of the patient ID is not `patient_ id`
#' in the `patient_ data`, you need to use `patient_ id` as a parameter to
#' specify the patient ID column. In addition, when denominator_data is also a
#' dataframe, the patient ID column names in denominator_data and numerator_data
#' are required to be the same.
#'
#' @param ifunite default `FALSE`.when `ifunite = TRUE`，return a character
#'  contain `count_patient_num(proportion of patients)`.
#'
#' @return The number and proportion of patients
#'
#' @export
#'
#' @examples
#' data(data_med)
#' pat_small = data_med$pat %>% calcu_age(.,birthdate) %>% dplyr::as_tibble(.) %>%  filter("age">30)
#' pat_table = data_med$pat
#' pop_perc(pat_small,1000)



pop_perc = function(numerator_data,denominator_data,patient_id=NULL,ifunite = FALSE){

  test_pat_id = "patient_id" %in% colnames(numerator_data)

  out = if( "numeric" %in% is(denominator_data) & is.null(patient_id) & test_pat_id == TRUE){
    out =list()
    out$pat = pe(numerator_data)
    out$per = fr(out$pat/denominator_data)
    out
  }else if("numeric" %in% is(denominator_data)  & !is.null(patient_id)){
    out =list()
    out$pat = pe_st(numerator_data,patient_id)
    out$per = fr(out$pat/denominator_data)
    out
  }else if("data.frame" %in% is(denominator_data) &
           is.null(patient_id) & test_pat_id == TRUE){
    out =list()
    out$pat = pe(numerator_data)
    out$per = fr(out$pat/pe(denominator_data))

  }else if("data.frame" %in% is(denominator_data) &
           !is.null(patient_id)){
    out =list()
    out$pat = pe_st(numerator_data,patient_id)
    out$per = fr(out$pat/pe_st(denominator_data,patient_id))
    out
  }else{
    message("There is an error in the internal function.
          Please check the relevant parameter settings according to the description.")
  }

  if(ifunite==TRUE & !is.null(out)){
    paste0(out$pat,"(",out$per,")")
  }else{out}

}

############## Function assist -----

fr = function(data){
  data = as.numeric(data)
  return(
    paste0(format(round(data*100,2),nsmall=2),"%",sep="")
  )
}

pe_st =  function(data,raw_patient_id){
  if (is.null(data)) {
    stop("input datatable is wrong,please recheck data")
  }else if(!"patient_id" %in% colnames(data) & is.null(raw_patient_id)){
    stop("Please recheck colnames,
         or add a new column name that specifies the patient iD")
  }else if(!"patient_id" %in% colnames(data) &
           !is.null(raw_patient_id) &
           raw_patient_id %in% colnames(data)){
    return(data %>% select(raw_patient_id) %>% dplyr::n_distinct(.))
  }else if("patient_id" %in% colnames(data)){
    return(data %>% select(patient_id) %>% dplyr::n_distinct(.))
  }else{
    return(message("Unknown error occurred in dataset. \nPlease recheck again"))
  }}



