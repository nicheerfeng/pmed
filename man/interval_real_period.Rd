% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_real_period.R
\name{interval_real_period}
\alias{interval_real_period}
\title{Calculate the non overlapping actual medication days of patients in the intersection of multiple times.}
\usage{
interval_real_period(
  data,
  patient_id = "patient_id",
  st_dates = "prec_start_date",
  end_dates = "prec_end_date"
)
}
\arguments{
\item{data}{prescription patient table}

\item{patient_id}{a  "patient_id" column name or a similar column}

\item{st_dates}{a  "Prescription start time" column name or a similar column}

\item{end_dates}{a  "Prescription end time" column name or a similar column}
}
\value{
Actual non overlapping medication days per patient
}
\description{
Calculate non overlapping time.
}
\details{
At least three column names must be given, including "patient id", "prescription start time","prescription end time".
}
\examples{
\dontrun{
## Build data
prec_data = data_med$prec \%>\%
  mutate(prec_end_date = as.Date(prec_date) + sample(1:30,1)) \%>\%
  rename(prec_start_date = prec_date)

interval_real_period(prec_data,patient_id = "patient_id",
                     st_dates = "prec_start_date",
                     end_dates = "prec_end_date")
}
}
