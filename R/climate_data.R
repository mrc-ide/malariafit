#' @title Get climate satellite data for a site
#' @description Returns daily population-weighted CHIRPS total rainfall for data_type = "rainfall".
#' Returns daily ERA5 mean 2m temperature when data_type = "temperature"
#' @param site Site object
#' @param name_2 Name of admin 2 region. If unspecified, climate data for admin 1 will be returned.
#' @param data_type A vector of required types of data. "rainfall" and "temperature" are accepted. Other data types must be given by a specific title e.g. "ERA5_Land_specific_humidity"
#' @param simplify_outputs If TRUE, outputs will be simplified to just date and unweighted data
#' @param data_directory Directory where the climate data is located
#' @importFrom utils read.csv
#' @export
get_climate_data <- function(site,
                             name_2 = NULL,
                             data_type = c("rainfall", "temperature"),
                             simplify_outputs = FALSE,
                             data_directory = "//wpia-hn.hpc.dide.ic.ac.uk/vimc-cc/DataCentre/observation/countries/") {
  allowed <- c("rainfall", "temperature")
  if (simplify_outputs && any(! data_type %in% allowed)) {
    stop("simplify_outputs is currently only available for temperature and rainfall outputs")
  }
  data_type[data_type == "rainfall"] <- "CHIRPS_total_precipitation"
  data_type[data_type == "temperature"] <- "ERA5_Land_2m_temperature"
  gadm_code <- get_gadm_code(site, name_2)
  iso <- substr(gadm_code, 1, 3)
  num <- sub(".*\\.", "", gadm_code)
  fn_init <- paste0(iso, "_v410_", num, "_")
  path <- paste0(
    data_directory,
    iso,
    "/"
  )
  years <- c("_1990_1999.csv",
             "_2000_2009.csv",
             "_2010_2019.csv",
             "_2020_2029.csv")
  out_df <- data.frame()
  for (year in years) {
    fns <- paste0(path, fn_init, data_type, year)
    mid_df <- do.call(
      cbind,
      lapply(fns, read.csv, check.names = FALSE)
    )
    out_df <- rbind(out_df, mid_df)
  }
  out_df <- out_df[, !duplicated(names(out_df))] #Remove any duplicated columns
  out_df$Date <- as.Date(out_df$Date)
  if(simplify_outputs == TRUE){
    out_df <- out_df[,colnames(out_df) %in% c("Date","CHIRPS_cos", "ERA5_Land_cos")]
    colnames(out_df)[colnames(out_df) == "CHIRPS_cos"] <- "Rainfall"
    colnames(out_df)[colnames(out_df) == "ERA5_Land_cos"] <- "Temperature"
  }
  return(out_df)
}

#' @title Set carrying capacity according to White et al. 2011
#' @description Carrying capacity proportional to past rainfall weighted by an exponential distribution
#' @param rainfall_df DataFrame with containing columns 'Date' and 'Rainfall'. Output from get_climate_data(.., simplify_outputs = TRUE)
#' @param tau smoothing factor. Larger values indicate greater smoothing.#
#' @importFrom stats filter
#' @export
#'
carrying_capacity_white <- function(rainfall_df, tau) {
  rain  <- rainfall_df$Rainfall
  T     <- length(rain)
  alpha <- exp(-1 / tau)

  num <- stats::filter(rain,
                       filter = alpha,
                       method = "recursive",
                       init = rain[1])
  num <- as.numeric(num)

  denom <- tau * (1 - alpha^(1:T))

  #Compute and normalize
  K      <- num / denom
  Knorm  <- K / mean(K, na.rm = TRUE)
  Knorm[Knorm < 0.001] <- 0.001
  data.frame(date = rainfall_df$Date,
             K    = Knorm)
}

#' @title Set carrying capacity according to Imperial College London model
#' @param rainfall_df DataFrame containing columns 'Date' and 'Rainfall'. Output from get_climate_data(.., simplify_outputs = TRUE)
#' @param tau Smoothing factor. Larger values indicate greater smoothing and generally lead to a longer mosquito season
#' @param zsat Carrying capacity saturation. Larger values of zsat limit maximum carrying capacity.
#' @param zmax Washout effects
#' @export
carrying_capacity_icl <- function(rainfall_df, tau, zsat, zmax) {
  K <- vector(length = nrow(rainfall_df))
  K[1] <- 1
  Z <- rainfall_df$Rainfall / mean(rainfall_df$Rainfall)
  for(t in 1:(nrow(rainfall_df)-1)){
    K[t+1] <- max(K[t] + Z[t]/tau - (K[t] / tau)*(1 + Z[t] / zsat + (Z[t] / zmax)^2), 0)
  }
  Knorm <- K / mean(K, na.rm = TRUE)
  Knorm[Knorm < 0.001] <- 0.001
  data.frame(date = rainfall_df$Date, K = Knorm)
}

#' @title Get GADM code for a particular site
#' @param site Site object of admin 1 region
#' @param name_2 Name of admin 2 region within site. If Null, admin 1 returned.
#' @importFrom dplyr filter
#' @export
get_gadm_code <- function(site, name_2 = NULL){
  iso3c <- site$sites$iso3c
  name_1 <- site$sites$name_1
  df <- get("mapping_df", envir = asNamespace("malariafit")) |>
    dplyr::filter(GID_0 == iso3c) |>
    dplyr::filter(NAME_1 == name_1)
  if(is.null(name_2)){
    out <- df$GID_1[1]
  } else{
    if(!name_2 %in% df$NAME_2) stop(message("Named admin2 region not found within site"))
    out <- df$GID_2[df$NAME_2 == name_2][1]
  }
  return(out)
}

