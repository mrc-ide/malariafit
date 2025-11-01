#' An example country site file
#'
#' A site file containing all of the input components for running a setting-specific
#' \href{https://mrc-ide.github.io/malariasimulation/}{malariasimulation} model run.
#'
#' @format A list with 10 variables:
#' \describe{
#'   \item{country}{The country name}
#'   \item{version}{Site file version}
#'   \item{admin_level}{The levels of spatial disaggregation}
#'   \item{sites}{Unique sites}
#'   \item{shape}{Sptial boundaries (set to NA in testing shapefile)}
#'   \item{cases_deaths}{Epidemiological site data}
#'   \item{prevalence}{Prevalence data}
#'   \item{interventions}{Intervention coverage and specification}
#'   \item{population}{Population, population at risk mand age-disaggregated population projections}
#'   \item{demography}{Demographic projections}
#'   \item{vectors}{Vector species and pyrethroid resistance}
#'   \item{seasonality}{Seasonal profile parameters, monthly rainfall and fourier predictions}
#'   \item{blood_disorders}{Blood disorder data}
#'   \item{accessibility}{Accessibility data}
#'   \item{eir}{Calibrated eir}
#' }
"example_site"

#' An example list of MCMC outputs
#'

#'
#' @format A list of four monty output objects
"example_samples_list"

#'
#'@format Example rainfall dataframe
"example_rainfall_data"
