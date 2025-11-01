#' @title Get malariasimple basic parameters for site
#' @description Sets up basic malariasimple parameters for a specific site. Equivalent to malariasimple::get_parameters().
#' Population is taken from site-files and is set to the mean average over the defined simulation years.
#' @param site site object
#' @param start_date Start date of simulation in "YYYY-MM-DD" format. 1st January 2000.
#' @param end_date End date of simulation in "YYYY-MM-DD" format. Default is 31st December 2020.
#' @param ... Further arguments to malariasimple::get_parameters()
#' @importFrom dplyr filter
#' @export

basic_parameters_site <- function(site,
                                  start_date = "2000-01-01",
                                  end_date = "2020-12-31",
                                  ...) {

  start_date <- to_date(start_date, "start_date")
  end_date <- to_date(end_date, "end_date")

  if (start_date <  as.Date("1900-01-01") | start_date >  as.Date("2500-12-31")) stop("start_date must be between 1900-01-01 and 2500-31-12. Have you entered in the format 'YYYY-MM-DD'?")
  if (end_date <  as.Date("1900-01-01")  | end_date >  as.Date("2500-12-31")) stop("end_date must be between 1900-01-01 and 2500-31-12. Have you entered in the format 'YYYY-MM-DD'?")

  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  leap_days <- n_leap_days(start_date, end_date)
  n_days <- as.integer(end_date - start_date) + 1 - leap_days #Assume all years have 365 days.


  human_pop_df <- site$population$population_total |> dplyr::filter(year %in% start_year:end_year)
  human_population <- round(mean(human_pop_df$pop))


  params <- malariasimple::get_parameters(
    human_pop = human_population,
    n_days = n_days,
    ...
  )
  params$start_date <- start_date
  params$start_year <- start_year
  params$end_year <- end_year
  params$end_date <- end_date

  return(params)
}

#' @title Set site-specific SMC and ITN interventions
#' @description Helper function for malariasimple input parameter list.
#' Takes as input current malariasimple parameters and a site object.
#' Returns updated malariasimple parameters
#' @param params malariasimple parameters
#' @param site site object
#' @export
set_interventions_site <- function(params, site) {
  interventions <- site$interventions
  seasonality <- site$seasonality$seasonality_parameters
  start_year <- params$start_year
  interventions <- interventions[interventions$year >= start_year, ]

  end_year <- params$end_year
  interventions <- interventions[interventions$year <= end_year, ]

  smc_present <- sum(interventions$smc_cov) != 0
  itn_present <- sum(interventions$itn_use) != 0
  irs_present <- max(interventions$irs_cov) > 0.05
  if (irs_present)
    warning("Warning. IRS usage over 5% for site. IRS is not currently included in the model")

  if (itn_present)
    params <- set_bednets_site(params, interventions)
  if (smc_present)
    params <- set_smc_site(params, interventions, seasonality)
  return(params)
}


#' @title Set site-specific ITN interventions
#' @param params malariasimple parameters
#' @param interventions 'interventions' item from site object
#' @importFrom malariasimple set_bednets
#' @export
set_bednets_site <- function(params, interventions){
  baseline_year <- min(interventions$year)
  # If not specified, assume distribution happens January 1st
  if(!"itn_distribution_day" %in% colnames(interventions)){
    interventions$itn_distribution_day <- 1
  }
  timesteps <- interventions$itn_distribution_day + (interventions$year - baseline_year) * 365

  # Net retention half life does not vary over time (Should match what is used when fitting input dist)
  retention <- unique(interventions$mean_retention)
  if(length(retention) > 1){
    stop("Time-varying net rentetion is not currently supported")
  }

  # Net input coverage
  coverages <- interventions$itn_input_dist
  coverages[is.na(coverages)] <- 0

  # Net efficacy parameters
  dn0 <- interventions$dn0
  rn <- interventions$rn0
  rnm <- interventions$rnm
  gamman <- interventions$gamman * 365

  params <- malariasimple::set_bednets(
    params,
    days = timesteps,
    coverages = coverages,
    retention = retention,
    dn0 = dn0,
    rn = rn,
    rnm = rnm,
    gamman = gamman
  )

  return(params)
}

#' @title Set site-specific SMC interventions
#' @param params malariasimple parameters
#' @param interventions 'interventions' item from site object
#' @param seasonality 'seasonality' item from site object
#' @importFrom malariasimple get_peak_cc set_smc
#' @export
set_smc_site <- function(params, interventions, seasonality){
  month <- 365 / 12
  baseline_year <- min(interventions$year)
  if(!all(interventions$smc_drug == "sp_aq")){
    stop("Not currently set up for non SP AQ SMC drug")
  }
  #Day-of-year of peak seasonality
  peak <- get_peak_cc(g0 = seasonality$g0,
                                     g = c(seasonality$g1, seasonality$g2, seasonality$g3),
                                     h = c(seasonality$h1, seasonality$h2, seasonality$h3))
  rounds <- interventions$smc_n_rounds
  if(sum(rounds) == 0) stop("SMC is indicated, but site is showing zero rounds")
  year_start_times <-  1 + (interventions$year - baseline_year) * 365
  peak_season_times <- peak + year_start_times
  smc_period <- 30 #Assume 30 days between each SMC distribution
  smc_days <- Map(function(r, peak) {
    smc_length <- smc_period * (r - 1)
    day1 <- peak - smc_length / 2
    return(seq(
      from = day1,
      by = smc_period,
      length.out = r
    ))
  }, rounds, peak_season_times)
  smc_days <- smc_days |> unlist() |> round()
  smc_days <- smc_days[smc_days > 0]
  coverages <- rep(interventions$smc_cov, rounds)
  coverages <- coverages[smc_days < params$n_days]
  smc_days <- smc_days[smc_days < params$n_days]
  min_age <- interventions$smc_min_age |> mean() |> round() ##UPDATE WHEN MALARIA SIMPLE ALLOWS VARYING MIN/MAX SMC AGE
  max_age <- interventions$smc_max_age |> mean() |> round()

  #This little fudge makes things much easier will have to do for now
  closest_min_age <- params$age_vector[which.min(abs(params$age_vector - min_age))]
  closest_max_age <- params$age_vector[which.min(abs(params$age_vector - max_age))]

  params <- malariasimple::set_smc(
    params = params,
    drug = "SP_AQ",
    days = smc_days,
    coverages = coverages,
    min_age = closest_min_age,
    max_age = closest_max_age,
    distribution_type = "random" #Assume random distribution
  )
  params$smc_days <- smc_days
  return(params)
}

#' @title Set site-specific seasonality
#' @param params malariasimple parameters
#' @param site site object
#' @importFrom malariasimple set_seasonality
#' @export
set_seasonality_site <- function(params, site){
  seasonality <- site$seasonality$seasonality_parameters
  params <- params |>
    malariasimple::set_seasonality(g0 = seasonality$g0,
                    g = c(seasonality$g1, seasonality$g2, seasonality$g3),
                    h = c(seasonality$h1, seasonality$h2, seasonality$h3))
  return(params)
}

#' @title Set site-specific equilibrium
#' @param params malariasimple parameters
#' @param site site object
#' @importFrom malariasimple set_equilibrium
#' @export
set_equilibrium_site <- function(params, site){
  init_eir <- site$eir$eir
  params <- params |>
    malariasimple::set_equilibrium(init_eir)
}

#' @title Set site-specific manual carrying capacity input
#' @param params malariasimple parameters
#' @param rainfall_df DataFrame containing columns 'Date' and 'Rainfall'. Obtained using get_climate_data(..data_type = "precipitation")
#' @param model Rainfall model to be used. Either "white" or "icl"
#' @param tau Both models require a specification for tau
#' @param zsat 'icl' model requires a value for zsat
#' @export
set_rainfall_site <- function(params,
                              rainfall_df,
                              model = "white",
                              tau = NULL,
                              zsat = NULL,
                              zmax = NULL) {
  if (model == "white") {
    if (is.null(tau))
      stop(message("When model = 'white', tau must be specified"))
    cc_df <- carrying_capacity_white(rainfall_df, tau)
  } else if (model == "icl") {
    if (is.null(tau))
      stop(message("When model = 'icl', tau must be specified"))
    if (is.null(zsat) | is.null(zmax))
      stop(message("When model = 'icl', zsat and zmax must be specified"))
    cc_df <- carrying_capacity_icl(rainfall_df, tau, zsat, zmax)
  }
  cc_df <- cc_df[cc_df$date >= params$start_date, ]
  cc_df <- cc_df[cc_df$date <= params$end_date, ]
  params <- params |> malariasimple::set_rainfall_manual(cc_ts = cc_df$K)
  return(params)
}

#' @title Plot bednet usage
#' @export
plot_bednet_usage <- function(site){
  interventions <- site$interventions
  site_name = paste0(interventions$name_1[1], ", ", interventions$country[1])
  plot(interventions$year, interventions$itn_use,
       type = "l",
       ylab = "ITN Usage",
       xlab = "Year",
       main = site_name)
}

#' @title Plot SMC usage
#' @export
plot_smc_usage <- function(site){
  interventions <- site$interventions
  site_name = paste0(interventions$name_1[1], ", ", interventions$country[1])
  plot(interventions$year, interventions$smc_cov,
       type = "l",
       ylab = "SMC Usage",
       xlab = "Year",
       main = site_name)
}

#' @title Plot IRS usage
#' @export
plot_irs_usage <- function(site){
  interventions <- site$interventions
  site_name = paste0(interventions$name_1[1], ", ", interventions$country[1])
  plot(interventions$year, interventions$irs_cov,
       type = "l",
       ylab = "IRS Usage",
       xlab = "Year",
       main = site_name)
}

#' @title Plot all interventions
#' @export
plot_interventions <- function(site){
  old_par <- par(no.readonly = TRUE) # Save current settings
  on.exit(par(old_par)) # Restore settings when function exits

  par(mfrow = c(3, 1)) # 3 rows, 1 column

  plot_bednet_usage(site)
  plot_smc_usage(site)
  plot_irs_usage(site)
}
