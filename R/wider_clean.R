#' Long table to width table, for patients' drug list and diagnosis table
#'
#' Usually each line of the patient's drug list is a separate drug name, while
#' each row of the patient's diagnosis table is a combined string of multiple diagnoses.
#' Therefore, it is necessary to develop a stream form to carry out efficient
#' length-width conversion based on the patient's visit ID, so as to facilitate the
#' descriptive statistics of the downstream process.
#'
#' `ifunion` parameter provides the user with an indication of whether the column
#' used for length-width conversion is in the form of a merged string. When this
#' colunm is a merged string (usually diagnosis table), it needs to be deduplicated
#' based on the group ID expansion data and separate a collapsed column into
#' multiple rows,see [separate_rows()].
#'
#'
#' @param data a dataframe
#'
#' @param aim_select three column names for analysis, including patient ID,
#' visiting ID, and column names that need to change from length to width, the
#' user is required to give the column name in the specified order.
#'
#' @param ifunion used to indicate whether the column that needs to be long to
#' wide is a merged string. See more for details
#'
#' @param split default is ",".When the `ifunion = TRUE`,`split` provides a split
#'  point for data deduplication.
#'
#' @return A unique value for each patient, such as whether each patient has
#' suffered from a disease or used a drug.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_med)
#' diags = data_med$diag
#' aim_select = c("patient_id","visit_id","diag_union")
#' diag_wider = wider_clean(diags,aim_select)
#'
#' prec = data_med$prec
#' aim_select = c("patient_id","visit_id","prec_name")
#' prec_wider= wider_clean(prec,aim_select,ifunion = FALSE)
#'}
wider_clean = function(data,aim_select,ifunion = TRUE,split = ","){

  aim_select_clean = c("patient_id","visit_id","clean")

  if(ifunion == TRUE){
    data %>%
      select(aim_select) %>% distinct() %>%
      rename_at(vars(names(.)) ,~ aim_select_clean) %>%
      replace_na(list(clean = "NA")) %>%
      group_by(visit_id) %>%
      str_uniq(clean,split = split) %>% ungroup() %>%
      tidyr::separate_rows(.,clean,sep=",") %>% distinct() %>%
      mutate(x =1) %>%
      spread(clean,x) %>% select(!c("visit_id","NA")) %>%
      replace(is.na(.),0) %>% distinct() %>%
      group_by(patient_id) %>%
      mutate(across(!starts_with("patient_id"), max,.names = "{.col}")) %>%
      ungroup() %>% distinct()
  }else{
    data %>%
      select(aim_select) %>% distinct() %>%
      rename_at(vars(names(.)) ,~ aim_select_clean) %>%
      replace_na(list(clean = "NA")) %>%
      mutate(x =1) %>%
      spread(clean,x) %>% select(!c("visit_id")) %>%
      replace(is.na(.),0) %>% distinct() %>%
      group_by(patient_id) %>%
      mutate(across(!starts_with("patient_id"), max,.names = "{.col}")) %>%
      ungroup() %>% distinct()
  }
}

##### Auxiliary function
str_uniq  = function(data,aim_name,split = ","){
  aim_name_ = enquo(aim_name)
  aim_name_cha = deparse(substitute(aim_name))
  split_cha = deparse(substitute(split))
  data %>%
    mutate(
      "{aim_name_cha}" :=  paste(unique(trimws(unlist(strsplit(!!aim_name_,
                                                               split = "{split}",fixed = F,perl = T)))),
                                 collapse =","))
}
