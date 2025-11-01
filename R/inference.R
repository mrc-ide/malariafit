#' @title Get MCMC input manual
#' @description A flexible function which takes a pre-defined prior and likelihood and creates a list for use in mcmc_go()
#' @param prior Monty_model object which outputs prior density
#' @param likelihood Monty_model object which outputs likelihood
#' @param sampler Monty sampler object
#' @param initial Vector of initial parameter values (in same order as prior and likelihood)
#' @param n_steps Number of MCMC iterations
#' @param n_chains Number of parallel MCMC chains
#' @export
get_mcmc_input <- function(prior,
                           likelihood,
                           sampler,
                           initial,
                           n_steps = 100,
                           n_chains = 2){
  posterior <- likelihood + prior
  mcmc_input_lists <- list(
    posterior = posterior,
    sampler = sampler,
    n_steps = n_steps,
    initial = initial,
    n_chains = n_chains)
}


#' @title Run MCMC
#' @description Wrapper function for monty::monty_sample. Having a single argument simplifies use of bulk cluster requests.
#' @param input_list Output from get_mcmc_input()
#' @importFrom monty monty_sample
#' @export
mcmc_go <- function(input_list){
  samples <- monty::monty_sample(model = input_list$posterior,
                                 sampler = input_list$sampler,
                                 n_steps = input_list$n_steps,
                                 initial = input_list$initial,
                                 n_chains = input_list$n_chains)
}


