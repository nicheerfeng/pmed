#' Calculate the age of the patient
#'
#' `RefDate` needs to use `Index Date` in most statistical scenarios
#'
#' @param data a dataframe contain  birth date,and alse may be exist index date columns.
#'
#' @param birth_date
#'       need a colname contain time sequence;
#'       maybe you need to transfer using [as.Date()].
#'
#' @param refDate
#'       Default is in today.
#'       `RefDate` needs to use `Index Date` in most statistical scenarios.
#'
#' @param unit
#'       Default is in years.
#'       Argument 'unit' must be one of 'year', 'month', 'week', or 'day'.
#'
#' @return add a numeric type column called `age`
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' data(data_med)
#' ## Calculate the age of the patient based on the present time
#' data_med$pat %>% calcu_age(.,birthdate)
#' ## Calculate the age of the patient based on the index date
#' data_med$pat %>% calcu_age(.,birthdate,index_date)
#'

calcu_age = function(data,birth_date,refDate = Sys.Date(), unit = "year"){

  birthdate = enquo(birth_date)
  refdate = enquo(refDate)

  birth_date_cha = deparse(substitute(birth_date))
  refDate_cha = deparse(substitute(refDate))

  test_birth = birth_date_cha %in% colnames(data)
  test_refDate = refDate_cha %in% colnames(data)

  if (is.null(data)) {
    stop("input datatable is null,please recheck data")
  }else if(test_refDate == FALSE & test_birth == TRUE){
    warning("Today's time is being used to calculate the patient's age.")
    data %>%
      tr(.,c(birth_date_cha),"dat") %>%
      mutate(age = calc_page(!!birthdate)) %>%
      mutate(across(.cols = c("age"), .fns = as.numeric))  %>%
      filter(age>0 & age<=100)

  }else if(test_refDate == TRUE & test_birth == TRUE){
    warning("`Index date` is being used to calculate the patient's age.")
    data %>%
      tr(.,c(birth_date_cha,refDate_cha),"dat") %>%
      mutate(age = calc_page(!!birthdate,!!refdate)) %>%
      mutate(across(.cols = c("age"), .fns = as.numeric))  %>%
      filter(age>0 & age<=100)
  }else{
    stop("input parameters maybe wrong , please recheck data")
  }
}


calc_page <- function(birthDate, refDate = Sys.Date(), unit = "year") {
  if (grepl(x = unit, pattern = "year")) {
    lubridate::as.period(interval(birthDate, refDate), unit = 'year')$year
  } else if (grepl(x = unit, pattern = "month")) {
    lubridate::as.period(interval(birthDate, refDate), unit = 'month')$month
  } else if (grepl(x = unit, pattern = "week")) {
    floor(lubridate::as.period(interval(birthDate, refDate), unit = 'day')$day / 7)
  } else if (grepl(x = unit, pattern = "day")) {
    lubridate::as.period(interval(birthDate, refDate), unit = 'day')$day
  } else {
    print("Argument 'unit' must be one of 'year', 'month', 'week', or 'day'")
    NA
  }}

