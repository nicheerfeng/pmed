#' Provide grouping tags for \code{\link{cut}}
#'
#' @param breaks_list data split points for grouping,such as
#'         \code{c(18,30,40,50,60,70,Inf)}
#'
#' @param digits  defines the number of decimal places for the return label.
#'
#' @param openr default `openright= FALSE`,the data interval is in the
#'  form of left opening and right closing,such as `[1,2)`.When `openright= TRUE`,
#'  the data interval is in the form of left closing and right opening.
#'
#' @return a grouping tags
#'
#' @export
#'
#' @examples
#' breaks_age <- c(18,30,40,50,60,70,Inf)
#' cut_label(breaks_age)
#' cut_label(breaks_age,2)
#'
cut_label <- function(breaks_list,openr = FALSE,digits =0){

  lapply(1:(length(breaks_list)-1),function(i){

    step1 = format(round(breaks_list[i],digits),nsmall=digits)

    step2 = format(round(breaks_list[i+1],digits),nsmall=digits)

    if(openr == FALSE){paste0('[',step1,",",step2,')')}else{
      paste0('(',step1,",",step2,']')}})  %>%

    do.call(c,.) %>% stringr::str_replace(.,"Inf","+")}

#' Convert one continuous variables into grouped variables
#'
#' Generate a new table containing classified columns from continuous variables according to the
#' grouping interval list provided by the user
#'
#' It is suitable for the interval segmentation of continuous data such as
#' patient age and some biochemical indexes.
#‘
#'
#' @param data a dataframe contain continuous type data column.
#'
#' @param breaks_list data split points for grouping such as
#'       \code{c(18,30,40,50,60,70,Inf)}.
#'
#' @param openr default `openright= FALSE`,the data interval is in the
#'  form of left opening and right closing,such as `[1,2)`.When `openright= TRUE`,
#'  the data interval is in the form of left closing and right opening.
#' @param sp_column the specified column name adapted to breaks_list,using a
#' non-standard normal form.
#'
#' @param digits  defines the number of decimal places for the return label.
#'
#' @return add a user-specified classified data column to the dataframe.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_med)
#' pat = data_med$pat %>% calcu_age(.,birthdate)
#'
#' breaks_age <- c(18,30,40,50,60,70,Inf)
#' mutate_class(pat,breaks_age,age,digits=2)
#' }
#'
mutate_class <- function(data,breaks_list,sp_column,openr = FALSE,digits =0){

  sp_column_cha = deparse(substitute(sp_column))

  labels = cut_label(breaks_list,openr,digits)

  sp_column_cut <- paste0(sp_column_cha,"_cut")

  temp <- data %>% mutate(
    "{sp_column_cut}" :=
      cut(
        eval(parse(text = sp_column_cha)) ,
        breaks = breaks_list,
        right = openr,
        include.lowest = TRUE,
        labels = labels) )
  return(temp)}


#'  Mutate multiple split variable columns
#'
#' @param data  a dataframe contain continuous type data column.
#'
#' @param lib_name_list a series of continuous variables for classification,
#' concatenated using character methods.
#'
#' @param list_cut data split points used for grouping in multiple continuous
#' variables,these split points need to be packed into a list.Warning：The order
#' of checking \verb{lib_name_list} and \verb{list_cut} before starting analysis is the same.
#'
#' @param openright default `openright= FALSE`,the data interval is in the
#'  form of left opening and right closing,such as `[1,2)`.When `openright= TRUE`,
#'  the data interval is in the form of left closing and right opening.
#'
#' @param digits  defines the number of decimal places for the return label.
#'
#' @return  adds multiple user-specified classified data columns to the data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_med)
#'
#' lab_wider = data_med$lab  %>%
#'   tr(.,c("test_date"),"dat") %>%
#'   group_by(patient_id,lab_name) %>%
#'   arrange(test_date) %>%  slice_tail(n =1) %>%  ungroup() %>%
#'   select(patient_id,lab_name ,lab_va) %>%
#'   tr(.,c("lab_va"),"num") %>%
#'   spread(lab_name,lab_va) %>%  distinct()
#'
#' names(lab_wider)
#'
#' HbA1c <- c(0,6.5,7.0,8.0,9.0,Inf)
#' TC <- c(0,5.2,6.2,Inf)
#' LDL <- c(0,3.4,4.1,Inf)
#' HDL <- c(0,1.0,Inf)
#' TG <- c(0,1.7,2.3,Inf)
#' WBC <- c(0,4,10,Inf)
#'
#' ## keep only the columns related to the analysis
#' lib_name_list <- names(lab_wider)[-1]
#' ## this step need to adjust the order of list subsets in name_list order
#' list_cut <- list(HbA1c,HDL,LDL,TC,TG,WBC)
#'
#' cut_dataframe = mmc(lab_wider,lib_name_list,list_cut,digits=2)
#'}

mmc = function(data, lib_name_list, list_cut, openright = FALSE,digits=0){
  tlist <- list()
  tlist$lib_name <- lib_name_list
  tlist$breaks <- list_cut

  out = lapply(1:length(lib_name_list), function(x){
    mutate_class_st(data,
                 tlist$breaks[[x]],tlist$lib_name[[x]],
                 openr = openright,digits= digits) })

  cbind(data,do.call(cbind,out))
}

## mmc auxiliary function
mutate_class_st <- function(data,breaks_list,sp_column,openr = FALSE,digits=0){

  labels = cut_label(breaks_list,openr,digits)

  sp_column_cut <- paste0(sp_column,"_cut")

  temp <- data %>% select(sp_column) %>% mutate(
    "{sp_column_cut}" :=
      cut(
        eval(parse(text = sp_column)) ,
        breaks = breaks_list,
        right = openr,
        include.lowest = TRUE,
        labels = labels) ) %>% select(sp_column_cut)
  return(temp)}



