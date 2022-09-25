#' packages of medical statistics
#'
#' List of commonly used R packages of medical statistics
#'
#' commonly：pacman::p_load(purrr,tidyverse,scales,dplyr,flextable,gt,gtsummary,
#' rstatix,forcats,janitor,table1,tableone,arrow,
#' data.table,datacleanr,stringr,tidyr,lubridate,openxlsx,gt)
#'
#' for exploratory:
#'    rstatix::get_summary_stats(linelist);
#'    dplur::ff_glimpse();
#'    skimr::skim(linelist);
#'    datacleanr::dcr_app(iris);
#'    summarytools::descr(iris);
#'
#'
#' for descriptive table:
#'    pacman::p_load(knitr,flextable,gt,gtsummary,tableone)
#'
#' for reactive table:
#'    pacman::p_load(DT)
#'
#' for speed up data analysis:
#'    pacman::p_load(data.table,arrow);
#'
#' for model:
#'    pacman::p_load(finalfit,broom)
#'
#' for plot:
#'  pacman::p_load(ggplot2,plotly)
#'
#'
#' @param data
#'   any param is ok!
#'
#' @return return a '?pmed::load_pkg'
#'
#' @export
#'
load_pkg = function(data){
  return(
    print("please use '?pmed::load_pkg'to get medical packages list")
  )}

