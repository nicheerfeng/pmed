#' @import dplyr
#' @import tibble
#' @import lubridate
#' @import parallel
#' @import gtsummary
#' @import tidyselect
#' @import methods
#' @importFrom stringr str_replace str_c
#' @importFrom rlang enquo
#' @importFrom utils globalVariables
#' @importFrom utils data
#' @importFrom pacman p_load
#' @importFrom purrr map map_df
#' @importFrom magrittr %>%
#' @importFrom tidyr spread separate_rows replace_na crossing separate gather
#' @importFrom stats sd
NULL


if(getRversion() >= "2.15.1") utils::globalVariables(c("!!",":=",".","*"))


## pe.R / calcu_age.R /
class_1 = c("patient_id","age")

## virtual_med_mulp.R:
class_2 = c("admission_date","rand","diag_name","visit_id")

## calcu_med.R : calcu() // bind_cut_cate() // bind_cut_cont()
class_3 =  c("var_label","variable",'label','SD','data','Mean','data_lab_calcu','SD2')

## virtual_med_mulp.R -- test()
class_4 = c("tidyverse","base","stringr","lubridate")

## wider_clean.R
class_5 = c('visit_id','clean','x','patient_id',"where")

null_list = purrr::map(1:5,function(x){get(paste0("class_",x))}) %>%
  do.call(c,.) %>% unique()

if(getRversion() >= "2.15.1") utils::globalVariables(null_list)



#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
