% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_sd_pop_perc.R
\name{pop_perc}
\alias{pop_perc}
\title{Calculate the number and proportion of patients}
\usage{
pop_perc(numerator_data, denominator_data, patient_id = NULL, ifunite = FALSE)
}
\arguments{
\item{numerator_data}{a dataframe subset}

\item{denominator_data}{a complete set of data, or a number used to represent}

\item{patient_id}{This function uses \verb{patient_ id} by default to calculate
the number of patients. If the column name of the patient ID is not \verb{patient_ id}
in the \verb{patient_ data}, you need to use \verb{patient_ id} as a parameter to
specify the patient ID column. In addition, when denominator_data is also a
dataframe, the patient ID column names in denominator_data and numerator_data
are required to be the same.}

\item{ifunite}{default \code{FALSE}.when \code{ifunite = TRUE}，return a character
contain \verb{count_patient_num(proportion of patients)}.}
}
\value{
The number and proportion of patients
}
\description{
Calculate the number and proportion of patients
}
\examples{
data(data_med)
pat_small = data_med$pat \%>\% calcu_age(.,birthdate) \%>\% dplyr::as_tibble(.) \%>\%  filter("age">30)
pat_table = data_med$pat
pop_perc(pat_small,1000)
}
