#' Multivariable type conversion
#'
#' Convert time, characters, numbers and factors;
#' Support tidy system;
#'
#' [tr()] Use the character definition variable method to convert one or more variables;
#' [tr_nst()] use non-standard variable definition methods to convert a variable type.
#'
#' @param data a dataframe contain  multiple variable types.
#'
#' @param sp_column
#'     The column name used to convert the variable type,
#'     use non-standard input in [tr_nst()].
#'
#' @param funss
#'    `"num"` : [base::as.numeric()];
#'    `"dat"`:  [base::as.Date()];
#'    `"cha"`:  [base::as.character()];
#'    `"fac"`:  [base::as.factor()].
#'    use non-standard input in [tr_nst()].
#'
#' @return Convert according to the specified type.
#'
#' @export
#'
#' @examples
#' data(data_med)
#' data_med$pat %>% tr(.,c("patient_id","visit_id"),"cha")
#' data_med$pat %>% tr_nst(.,patient_id,num)


tr = function(data,sp_column,funss){

  test_contain = sp_column %in% colnames(data)

  if (is.null(data)) {
    stop("input datatable is null,please recheck data")
  }else if("FALSE" %in% test_contain | is.null(funss)){
    stop("Please recheck colnames or other paramters")
  }else if(funss == "num"){
    return(
      data %>% mutate(across(.cols = sp_column, .fns = as.numeric)))
  }else if(funss == "dat"){
    return(
      data %>% mutate(across(.cols = sp_column, .fns = as.Date)))
  }else if(funss == "cha"){
    return(
      data %>% mutate(across(.cols = sp_column, .fns = as.character)))
  }else if(funss == "fac"){
    return(
      data %>% mutate(across(.cols = sp_column, .fns = as.factor)))
  }else{
    return("input function fasle")}}

#'
#' @rdname tr
#' @export
tr_nst = function(data,sp_column,funss){

  labname = enquo(sp_column)
  funname = deparse(substitute(funss))

  test_contain = deparse(substitute(sp_column)) %in% colnames(data)

  if (is.null(data)) {
    stop("input datatable is null,please recheck data")
  }else if("FALSE" %in% test_contain | is.null(funname)){
    stop("Please recheck colnames or other paramters")
  }else if(funname == "num"){
    return(
      data %>% mutate(across(.cols = !!labname, .fns = as.numeric)))
  }else if(funname == "dat"){
    return(
      data %>% mutate(across(.cols = !!labname, .fns = as.Date)))
  }else if(funname == "cha"){
    return(
      data %>% mutate(across(.cols = !!labname, .fns = as.character)))
  }else if(funname == "fac"){
    return(
      data %>% mutate(across(.cols = !!labname, .fns = as.factor)))
  }else{
    return("input function fasle")}}




