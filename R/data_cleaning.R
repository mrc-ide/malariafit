#' @title Filter sites by interventions usage
#' @description For each intervention, NULL (default) indicates ignore,
#' TRUE indicates intervention must be present,
#' FALSE indicates intervention must not be present
#' @param IRS Should IRS be present?
#' @param ITN Should ITN be present?
#' @param SMC Should SMC be present?
#' @param minimum_prop Minimum coverage at which intervention is considered 'present' (maximum value)
#' @export
filter_site_interventions <- function(site_list,
                        IRS = NULL,
                        ITN = NULL,
                        SMC = NULL,
                        minimum_prop = 0.05){
  clean_site_indicies <- vector()
  for(i in 1:length(site_list)){
    keep <- TRUE
    ints <- site_list[[i]]$interventions
    if(isTRUE(IRS)){
      if(max(ints$irs_cov) < minimum_prop) keep <- FALSE
    } else if(isFALSE(IRS)){
      if(max(ints$irs_cov) >= minimum_prop) keep <- FALSE
    }

    if(isTRUE(ITN)){
      if(max(ints$itn_use) < minimum_prop) keep <- FALSE
    } else if(isFALSE(ITN)){
      if(max(ints$itn_use) >= minimum_prop) keep <- FALSE
    }

    if(isTRUE(SMC)){
      if(max(ints$smc_cov) < minimum_prop) keep <- FALSE
    } else if(isFALSE(SMC)){
      if(max(ints$smc_cov) >= minimum_prop) keep <- FALSE
    }
    if(keep == TRUE) clean_site_indicies <- c(clean_site_indicies, i)
  }
  return(site_list[clean_site_indicies])
}
