#' Generate virtual datasets for multiple patients
#'
#' @param patient_n Generate the number of patients.
#' @param birth_start_date Minimum expected birth date of the patient.
#' @param birth_end_date Maximum expected birth date of the patient.
#' @param study_start_date
#'        The starting time of the baseline period specified in the study.
#' @param study_end_date
#'        The end time of the follow-up period specified in the study
#' @param average_year_visit_n
#'        The average number of visits by patients each year is 6 by default.
#'        The data are from the statistical records of the Chinese people.
#' @param ncores
#'        the number of cores used for parallelisation.
#' @return
#'    Returns a data set of multiple patients, including a doctor's table,
#'    prescription table, diagnosis table, and biochemical test table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_med_new = virtual_med_mulp(patient_n = 24,
#'    birth_start_date = "1980-01-01",birth_end_date ="2000-12-31" ,
#'    study_start_date = "2014-01-01",study_end_date ="2020-12-31",
#'    average_year_visit_n = 6,ncores=6)
#' }


virtual_med_mulp = function(patient_n,
                            birth_start_date,birth_end_date,
                            study_start_date,study_end_date,
                            average_year_visit_n,ncores=1){
  ## Calculate the virtual dataset of multiple patients：
  if(ncores == 1){
    data_out = Virtual_med_one(birth_start_date = birth_start_date,birth_end_date =birth_end_date ,
                               study_start_date = study_start_date,study_end_date =study_end_date,
                               average_year_visit_n = average_year_visit_n)
  }else{
    cl <- parallel::makeCluster(ncores)
    invisible(parallel::clusterEvalQ(cl, library("pmed")))
    data_out <- parallel::parLapply(cl,1:patient_n,function(x){
      Virtual_med_one(birth_start_date = birth_start_date,birth_end_date =birth_end_date ,
                      study_start_date = study_start_date,study_end_date =study_end_date,
                      average_year_visit_n = average_year_visit_n)})
    parallel::stopCluster(cl)
  }

  ## Construction of virtual ID for multiple patients：
  random_patient_id = lapply(1:patient_n,function(x){
    paste0(
      paste(sample(letters,size = 5,replace = T),
            collapse=""),
      sample(1000:6000,size = 1))}) %>% do.call(rbind,.) %>% as.character()

  ## Add a patient id to each patient's virtual pat data：
  list_group = function(data_list,random_patient_id){
    count_visit = lapply(1:length(data_list), function(x){dim(`[[`(data_list,x)$pat)[1]}) %>%
      do.call(c,.) %>% as.numeric()
    return( rep(random_patient_id,time = count_visit))    }

  ## Generate a virtual patient pat dataset containing patient ID：
  patient_id = list_group(data_out,random_patient_id)
  pat =  lapply(1:length(data_out),function(x){`[[`(data_out,x)$pat}) %>%
    do.call(rbind,.) %>%
    cbind(patient_id,.)

  prec = lapply(1:length(data_out),function(x){`[[`(data_out,x)$prec}) %>%
    do.call(rbind,.) %>% left_join(pat[,c("patient_id","visit_id")],by="visit_id")

  lab = lapply(1:length(data_out),function(x){`[[`(data_out,x)$lab}) %>%
    do.call(rbind,.) %>% left_join(pat[,c("patient_id","visit_id")],by="visit_id")

  diag = lapply(1:length(data_out),function(x){`[[`(data_out,x)$diag}) %>%
    do.call(rbind,.) %>% left_join(pat[,c("patient_id","visit_id")],by="visit_id")

  data = list(pat,prec,lab,diag)
  names(data) = c('pat',"prec","lab","diag")
  return(data)
}


#' calculate the virtual dataset of a patient
#'
#' @param birth_start_date Minimum expected birth date of the patient.
#' @param birth_end_date Maximum expected birth date of the patient.
#' @param study_start_date
#'        The starting time of the baseline period specified in the study.
#' @param study_end_date
#'        The end time of the follow-up period specified in the study
#' @param average_year_visit_n
#'        The average number of visits by patients each year is 6 by default.
#'        The data are from the statistical records of the Chinese people.
#' @return
#'    Returns a data set of a patients, including a doctor's table,
#'    prescription table, diagnosis table, and biochemical test table.
#'
#' @export
#'
#' @examples
#' Virtual_med_one(
#'  birth_start_date = "1980-01-01",birth_end_date ="2000-12-31" ,
#'    study_start_date = "2014-01-01",study_end_date ="2020-12-31",
#'    average_year_visit_n = 6)
#'
Virtual_med_one = function(birth_start_date,birth_end_date,
                           study_start_date,study_end_date,
                           average_year_visit_n){

  ## The total number of visits during the study period was estimated by
  ## calculating the duration of the whole study period and the annual
  ## average number of visits.
  study_interverl =
    as.period(interval(study_start_date,study_end_date), unit = 'year')$year

  max_visits = study_interverl * (average_year_visit_n+1)
  min_visits = study_interverl * (1)

  random_visit_id = lapply(1:sample(min_visits:max_visits,size =1),function(x){
    paste0(
      paste(sample(letters,size = 5,replace = T),
            collapse=""),
      sample(1000:6000,size = 1))}) %>% do.call(rbind,.) %>% as.character()

  ## Build a function to generate patient time data within a specified time range：
  g_rand_date = function(start_date,end_date,n,unit = "days"){

    start_dat <- as.POSIXct(start_date, format = '%Y-%m-%d', tz = 'Asia/Taipei')
    end_dat <- as.POSIXct(end_date, format = '%Y-%m-%d', tz = 'Asia/Taipei')

    date_Time <- sort(sample(seq(start_dat, end_dat, by = 'days'), n,replace = F))
    return(as.Date(date_Time))}

  pat =
    data.frame(
      visit_id = random_visit_id,
      sex = sample(c("men","female"),size =1),
      birthdate = g_rand_date(birth_start_date,birth_end_date,n=1) ,
      patient_type = sample(c("outp","Hosp"),size =length(random_visit_id),replace = T),
      admission_date =
        g_rand_date(study_start_date,study_end_date,n=length(random_visit_id))) %>%
    ## Calculate the discharge time of the patient:：
    mutate(Discharge_date = admission_date%m+% days(sample(3:45,size =1,replace = T)))


  ## Generate random lab table：----

  g_rand_lab = function(n){

    HbA1c = sample(1:12,size = 10,replace = T)
    ## Total_cholesterol
    TC = sample(1:7,size = 10,replace = T)
    ## Low_density_lipoprotein
    LDL = sample(1:5,size = 10,replace = T)
    ## High_density_lipoprotein
    HDL = sample(1:5,size = 10,replace = T)
    ## White_blood_cell
    WBC  = sample(1:20,size = 10,replace = T)
    ## Triglyceride
    TG = sample(1:20,size = 10,replace = T)

    lab_va_list = data.frame(HbA1c,TC,LDL,HDL,WBC,TG)

    lab_test_list = c("HbA1c","TC","LDL","HDL","WBC","TG")

    lab_name = sample(lab_test_list,size =sample(3:6,size =1),replace = F)

    lab_va = lab_va_list %>%
      select(lab_name) %>%
      filter(row_number() == sample(1:10,size =1)) %>%
      gather(.,"lab_name",.) %>% rename("lab_va" = "." )

    return(lab_va)
  }

  ## Lab name and lab value per visit；
  lab_visit = lapply(1:length(random_visit_id),function(x){g_rand_lab()})

  ## Used to merge lists and generate groups based on the length of each sublist：
  list_lab_group = function(data_list){
    lapply(1:length(data_list), function(x){dim(`[[`(data_list,x))[1]}) %>%
      do.call(rbind,.) %>% as.numeric()}

  ## Construct the lab data corresponding to each visit, and include the ID corresponding to the grouping.
  visit_lab = rep(random_visit_id,times =list_lab_group(lab_visit)) %>%
    cbind(.,do.call(rbind,lab_visit)) %>% rename("visit_id" = ".")

  lab = pat %>% select(c("visit_id","patient_type","admission_date")) %>%
    left_join(visit_lab,by="visit_id") %>%
    distinct() %>%
    ## Calculate the biochemical test time of patients：
    mutate(test_date = admission_date%m+% days(sample(c(0,1),size =1)))


  ## Generate a random diagnostic table：----
  g_rand_diag = function(n){
    diag_test_list = c(rep("diabetes",20),
                       rep(c("high blood pressure","Dyslipidemia","myocardial infarction","Stroke"),each = 10),
                       rep(c("Peripheral vascular disease","Symptomatic neuropathy"),each = 10),
                       rep(c( "Dementia","Chronic lung disease","Connective tissue disease","Peptic ulcers",
                              "Mild liver disease","Moderate and severe nephropathy","Lymphatic carcinoma"),each=5))

    diag_name = sample(diag_test_list,size =sample(3:10,size =1),replace = F)
    return(diag_name)}

  diag_visit = lapply(1:length(random_visit_id),function(x){g_rand_diag()})

  list_group = function(data_list){
    lapply(1:length(data_list), function(x){length(`[[`(data_list,x))[1]}) %>%
      do.call(rbind,.) %>% as.numeric()}

  visit_diag = rep(random_visit_id,times =list_group(diag_visit)) %>%
    cbind(.,do.call(c,diag_visit)) %>% data.frame() %>% rename("visit_id" = ".","diag_name" = "V2")


  diag = pat %>% select(c("visit_id","patient_type","admission_date")) %>%
    left_join(visit_diag,by="visit_id") %>%
    distinct() %>%
    mutate(diag_date = admission_date%m+% days(sample(c(0,1),size =1))) %>%
    mutate(rand = c(1:dim(.)[1])) %>%
    mutate(diag_name = ifelse(
      grepl(paste(sample(1:length(random_visit_id),size=5,replace = F),
                  collapse = "|"),rand),NA,diag_name)) %>%
    select(!rand) %>% distinct() %>%
    replace_na(list(diag_name = "NA")) %>%
    group_by(visit_id) %>%
    mutate(diag_union = str_c(diag_name,collapse = ",")) %>%
    ungroup()  %>% select(!c("diag_name")) %>%  distinct()

  ## Generate a random drug list：----

  g_rand_prec = function(n){
    prec_test_list = c("Biguanides","Sulfonylureas","Glinifera",
                       "insulin","antiplatelet medicines","Statins","ARBs",
                       "beta-blockers","Anticoagulants","Lipid-regulating drugs")

    prec_name = sample(prec_test_list,size =sample(3:10,size =1),replace = F)
    return(prec_name)}

  prec_visit = lapply(1:length(random_visit_id),function(x){g_rand_prec()})

  list_group = function(data_list){
    lapply(1:length(data_list), function(x){length(`[[`(data_list,x))[1]}) %>%
      do.call(rbind,.) %>% as.numeric()}

  visit_prec = rep(random_visit_id,times =list_group(prec_visit)) %>%
    cbind(.,do.call(c,prec_visit)) %>% data.frame() %>% rename("visit_id" = ".","prec_name" = "V2")


  prec = pat %>% select(c("visit_id","patient_type","admission_date")) %>%
    left_join(visit_prec,by="visit_id") %>%
    distinct() %>%
    mutate(prec_date = admission_date%m+% days(sample(c(0,1),size =1)))

  sum_test = list(pat,prec,lab,diag)
  names(sum_test) = c("pat","prec","lab","diag")

  return(sum_test)

}


