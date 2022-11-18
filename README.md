# pmed

<!-- badges: start -->
<!-- badges: end -->

## Goal: 

  An R package for building virtual datasets to simulate real world
  medical data in actual production environment, which are mainly used for 
  related research of RWD projects, including accelerating data analysis 
  and providing optimized batch functions.

## Installation

You can install the development version of pmed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nicheerfeng/pmed")
```

## Main function
### Auxiliary medical statistical function

**packages list**
``` r
# List of commonly used R packages of medical statistics
?pmed::load_pkg'
```

**Generate virtual datasets for multiple patients**
``` r
 data_med_new = virtual_med_mulp(patient_n = 24,
    birth_start_date = "1980-01-01",birth_end_date ="2000-12-31" ,
   study_start_date = "2014-01-01",study_end_date ="2020-12-31",
   average_year_visit_n = 6,ncores=6)
```
**Load virtual medical records for 50 virtual patients**
``` r
data(data_med)
```
**Quick conversion of common parameters**
``` r
data(data_med)
data_med$pat %>% tr(.,c("patient_id","visit_id"),"cha")
data_med$pat %>% tr_nst(.,patient_id,num)
```

**Calculate the average and standard deviation of continuous variables**
``` r
data(data_med)
pat_table = data_med$pat %>% calcu_age(.,birthdate)
mead_sd(pat_table,age)
pat %>% mean_sd(age)
pat %>% mean_sd(age,ifunion =TRUE)
```


### Aggregate function suitable for RWD descriptive statistics

**Quick statistics on the number of patients in the table**
``` r
datatable1 = data.frame(
patient_id = paste0("patent",1:100),
visit_id  = paste0("visit",1:100))

datatable2 = data.frame(
 patient = paste0("patent",1:100),
 visit_id  = paste0("visit",1:100))

pe(datatable1)

pe(datatable2,patient)
```
**Calculate the number and proportion of subsets**
``` r
data(data_med)
pat_small = data_med$pat %>% calcu_age(.,birthdate) %>% dplyr::as_tibble(.) %>%  filter("age">30)
pat_table = data_med$pat
pop_perc(pat_small,1000)
```



**Quickly generate the age of the patient**、
``` r
data(data_med)
## Calculate the age of the patient based on the present time
data_med$pat %>% calcu_age(.,birthdate)
## Calculate the age of the patient based on the index date
data_med$pat %>% calcu_age(.,birthdate,index_date)
```

**Generate label function developed for `base::cut()`**
``` r
breaks_age <- c(18,30,40,50,60,70,Inf)
cut_label(breaks_age)
cut_label(breaks_age,2)
```

**Calculate patient medication non overlapping time**
``` r
## Build data
prec_data = data_med$prec %>% 
  mutate(prec_end_date = as.Date(prec_date) + sample(1:30,1)) %>% 
  rename(prec_start_date = prec_date)

interval_real_period(prec_data,patient_id = "patient_id",
                     st_dates = "prec_start_date",
                     end_dates = "prec_end_date")
```


**Add corresponding classified data columns to one or more consecutive data** 
``` r
# for one
data(data_med)
pat = data_med$pat %>% calcu_age(.,birthdate)

breaks_age <- c(18,30,40,50,60,70,Inf)
mutate_class(pat,breaks_age,age,digits=2)

# for more
data(data_med)

lab_wider = data_med$lab  %>%
  tr(.,c("test_date"),"dat") %>%
  group_by(patient_id,lab_name) %>%
  arrange(test_date) %>%  slice_tail(n =1) %>%  ungroup() %>%
  select(patient_id,lab_name ,lab_va) %>%
  tr(.,c("lab_va"),"num") %>%
  spread(lab_name,lab_va) %>%  distinct()

names(lab_wider)

HbA1c <- c(0,6.5,7.0,8.0,9.0,Inf)
TC <- c(0,5.2,6.2,Inf)
LDL <- c(0,3.4,4.1,Inf)
HDL <- c(0,1.0,Inf)
TG <- c(0,1.7,2.3,Inf)
WBC <- c(0,4,10,Inf)

## keep only the columns related to the analysis
lib_name_list <- names(lab_wider)[-1]
## this step need to adjust the order of list subsets in name_list order
list_cut <- list(HbA1c,HDL,LDL,TC,TG,WBC)

cut_dataframe = mmc(lab_wider,lib_name_list,list_cut,digits=2)

```
**use `gtsummary` to faster descriptive statistics**
``` r
## Add a missing data to fully demonstrate the function of the function
lab_wider_cut = cut_dataframe %>%
  rbind(.,
        matrix(NA,nrow = 1,ncol =dim(lab_wider_cut)[2]) %>% data.frame() %>%
          rename_at(vars(names(.)) ,~ names(lab_wider_cut)) ) %>%
  mutate(patient_id = replace_na(patient_id, "test_id"))

## faster descriptive statistics.
data_lab_calcu <- calcu(lab_wider_cut[,-1],names(lab_wider)[-1])
```

**Extract the statistical summary generated by `calcu` for continuous data**

``` r
## Statistical results of a single indicator——test
bind_cut_cont_test = bind_cut_cont(data_lab_calcu,"HbA1c")

## Statistical results of multiple indicators
purrr::map_df(lib_name_list,bind_cut_cont,
              gt_table_body = data_lab_calcu,ppop=100)
```

**Extract the statistical summary generated by `calcu` for classified data**

``` r
data(data_med)
diags = data_med$diag
aim_select = c("patient_id","visit_id","diag_union")

diag_wider = wider_clean(diags,aim_select)

diag_wider_calcu = diag_wider %>% select(!"patient_id") %>%
  dplyr::mutate("vascular disease" =1) %>% calcu(.)

group_list = c("Dyslipidemia","high blood pressure","myocardial infarction")
merge_subgroup_name = "vascular disease"
#'
bind_cut_cate(diag_wider_calcu,group_list,merge_subgroup_name)
bind_cut_cate(diag_wider_calcu,group_list,ppop=1000)
```
