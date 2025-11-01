#' @title Get MCMC traceplot
#' @param pars_array 3d array of MCMC samples in the format [variable, iteration, chain]
#' @param pars Names of parameters to show. Default = all parameters
#' @param title Plot title
#' @param burn_in Length of 'burn-in' to discard from plot
#' @param true_vals_df Dataframe with two named columns: variable and value
#' @param save_file File location for ggsave
#' @param height Height of saved plot in cm
#' @param width Width of saved plot in cm
#' @importFrom reshape2 melt
#' @importFrom dplyr filter rename
#' @import ggplot2
#' @export
get_traceplot <- function(pars_array,
                          pars = NULL,
                          title = "",
                          burn_in = 0,
                          true_vals_df = NULL,
                          save_file = NULL,
                          height = 20,
                          width = 30) {
  samples <- pars_array |>
    reshape2::melt(varnames = c("variable", "iteration", "chain")) |>
    dplyr::filter(iteration > burn_in) |>
    dplyr::rename(sample_value = value)

  if (!is.null(pars))
    samples <- samples |> dplyr::filter(variable %in% pars)

  samples$chain <- as.factor(samples$chain)


  plt <- ggplot(data = samples) +
    geom_line(aes(
      x = iteration,
      y = sample_value,
      col = chain,
      group = chain
    )) +
    theme_bw() +
    labs(
      x = "Iteration",
      y = "Value",
      col = "Chain",
      title = title
    ) +
    facet_grid(vars(variable), scales = "free_y")

  if (!is.null(true_vals_df)) {
    samples <- dplyr::left_join(samples, true_vals_df)
    plt <- plt +
      geom_line(
        data = samples,
        aes(x = iteration, y = value, lty = "True value"),
        col = "black"
      ) +
      scale_linetype_manual(name = NULL, values = c("True value" = 2))
  }


  if (!is.null(save_file)) {
    ggsave(
      plot = plt,
      filename = save_file,
      width = width,
      height = height,
      units = "cm"
    )
  } else {
    return(plt)
  }
}

#' @title Get density plot of parameters
#' @param mcmc_output Output from mcmc_go() or monty::monty_sample()
#' @param pars Names of parameters to show. Default = all parameters
#' @param title Plot title
#' @param burn_in Length of 'burn-in' to discard from plot
#' @param true_vals_df Dataframe with two named columns: variable and value
#' @param priors Named list of functions. Each element should be a function taking a numeric vector x and returning the prior density at x. Names must match parameter names.
#' @param save_file File location for ggsave
#' @param height Height of saved plot in cm
#' @param width Width of saved plot in cm
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' @import ggplot2
#' @export
get_dens_plot <- function(pars_array,
                          pars = NULL,
                          title = "Parameter Posterior Density",
                          burn_in = 0,
                          true_vals_df = NULL,
                          priors = NULL,
                          save_file = NULL,
                          height = 20,
                          width = 30) {
  samples <- pars_array |>
    reshape2::melt(varnames = c("variable", "iteration", "chain")) |>
    dplyr::filter(iteration > burn_in)

  samples$chain <- as.factor(samples$chain)

  if (!is.null(pars)){
    samples <- samples |> dplyr::filter(variable %in% pars)
  }

  plt <- ggplot() +
    geom_density(
      data = samples,
      aes(
        x = value,
        color = chain,
        fill = chain,
        group = chain
      ),
      alpha = 0.3,
      size = 0.5
    )

  prior_df <- NULL
  if (!is.null(priors)) {
    prior_list <- lapply(names(priors), function(var) {
      if (!var %in% unique(samples$variable))
        return(NULL)
      vals <- samples$value[samples$variable == var]
      # sequence over sample range
      x_seq <- seq(min(vals), max(vals), length.out = 200)
      data.frame(variable = var,
                 x = x_seq,
                 prior = priors[[var]](x_seq))
    })
    prior_df <- do.call(rbind, prior_list)
    plt <- plt +
      geom_line(data = prior_df, aes(x = x, y = prior, lty = "Prior"))
  }

    if(!is.null(true_vals_df)){
      plt <- plt +
        geom_vline(data = true_vals_df %>% filter(variable %in% unique(samples$variable)), aes(xintercept=value, lty = "True Value"))
    }

    plt <- plt +
      facet_wrap(~ variable, scales = "free") +
    labs(
      title = title,
      x     = "Parameter value",
      y     = "Density",
      color = "Chain",
      fill  = "Chain",
      lty = ""
    ) +
    scale_linetype_manual(values = c("Prior" = 2,
                                       "True Value" = 1)) +
    theme_minimal(base_size = 14)

    if (!is.null(save_file)) {
      ggsave(
        plot = plt,
        filename = save_file,
        width = width,
        height = height,
        units = "cm"
      )
    } else {
      return(plt)
    }
}

#' @title Restitch Array
#' @description This function will take a list of parallel MCMC outputs, and combine the array items into a single array.
#' Useful for viewing results of multiple chains if you have performed parallel MCMC chains as separate simulations.
#' @param samples_list List of MCMC outputs from monty::monty_sample or malariafit::mcmc_go
#' @param var Variable of interest. Either "pars" or "density".
#' @importFrom abind abind
#' @export
restitch_array <- function(samples_list, var = "pars"){
  all_arrs <- lapply(samples_list, `[[`, var)
  combined_arr <- abind(all_arrs, along = 3)
}

#' @title Convert to form for use with bayesplot package
#' @param pars_array 3D array in the form [parameter, iteration, chain]. Output from monty_sample()$pars
#' @export
bplotify <- function(pars_array){
  aperm(pars_array, c(2,3,1))
}

#' @title Convert to form for use with coda package
#' @param pars_array 3D array in the form [parameter, iteration, chain]. Output from monty_sample()$pars
#' @importFrom coda mcmc.list
#' @export
codafy <- function(pars_array){
  list <- lapply(X = 1:dim(pars_array)[3],
                      FUN = function(x) mcmc(t(pars_array[,,x])))
  mcmc_list <- mcmc.list(list)
  return(mcmc_list)
}


