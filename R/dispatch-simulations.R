#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


#' Dispatch Simulations
#' 
#' @description Runs optic simulations
#'
#' @param object Simulation scenarios object created using `optic_simulation`
#' @param seed Specified as either NULL or a numeric. Sets a seed, which is becomes an index in results, for each independent set of simulations in `optic_simulation`.
#' @param use_future Runs simulation scenarios in parallel. Default FALSE, set to TRUE if you would like for the iterations to be run in parallel.
#' @param verbose Default 0. If greater than 0, provides details on what is currently running.
#' @param graceful If TRUE, errors in iterations are caught and retried up to 10 percent of total iterations. If FALSE, errors are not caught and will stop execution. Default is FALSE.
#' @param ... Additional parameters to be passed to model call.
#'
#' @return A single `data.frame` containing estimated treatment effects and summary statistics by model and draw, bound together to allow for different column names.
#' @export
dispatch_simulations <- function(object, seed=NULL, use_future=FALSE, verbose=0, graceful=FALSE, ...) {
  stopifnot("OpticSim" %in% class(object))
  
  # Create experimental design combining simulation_params with iterations
  experimental_design <- object$simulation_params %>%
    dplyr::mutate(param_id = dplyr::row_number()) %>%
    tidyr::expand_grid(iter = 1:object$iters) %>%
    dplyr::select(param_id, iter, dplyr::everything())
  
  total_runs <- nrow(experimental_design)
  
  if (verbose > 0) {
    cat(paste("TOTAL RUNS:", total_runs, "\n"))
    cat(paste("SIMULATION SCENARIOS:", nrow(object$simulation_params), "\n"))
    cat(paste("ITERATIONS PER SCENARIO:", object$iters, "\n"))
    if (use_future) {
      cat("DISPATCH METHOD: parallel (future)\n")
    } else {
      cat("DISPATCH METHOD: single-threaded\n")
    }
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Common function to run a single iteration
  run_single_iteration <- function(i) {
    param_id <- experimental_design$param_id[i]
    iter <- experimental_design$iter[i]
    single_simulation <- object$setup_single_simulation(param_id)
    sim_params <- object$simulation_params[param_id, ]
    
    run_single_sim_iteration(single_simulation, iter, graceful, sim_params, object$iters)
  }
  
  if (use_future) {
    # default progress bar with eta estimation
    results <- progressr::with_progress({
      p <- progressr::progressor(steps = total_runs)
      future.apply::future_lapply(
        1:total_runs,
        FUN = function(i) {
          p()
          run_single_iteration(i)
        },
        future.seed = seed,
        ...
      )
    })
  } else {
    # Sequential execution with progress bar
    results <- progressr::with_progress({
      p <- progressr::progressor(steps = total_runs)
      lapply(1:total_runs, function(i) {
        p()
        run_single_iteration(i)
      })
    })
  }
  
  # Bind all results together using dplyr::bind_rows to handle different column names
  final_results <- dplyr::bind_rows(results)
  
  # Add seed information if provided
  if (!is.null(seed)) {
    final_results$seed <- seed
  }
  
  return(final_results)
}

print_simulation_parms <- function(sim_params) {
  cat("Simulation parameters:\n")
  for (p in names(sim_params)) {
    cat(paste0("  ", p, ": ", sim_params[[p]], "\n"))
  }
}

# Helper for running a single simulation iteration, with optional graceful error handling
run_single_sim_iteration <- function(single_simulation, iter, graceful, sim_params, total_iters) {
  if (!graceful) {
    r <- run_iteration(single_simulation)
  } else {
    failed_attempts <- 0
    complete <- 0
    max_attempts <- ceiling(0.10 * total_iters)
    
    while (complete < 1 & failed_attempts < max_attempts) {
      r <- tryCatch({
        run_iteration(single_simulation)
      }, error = function(e) e)
      
      if (!"error" %in% class(r)) {
        complete <- complete + 1
      } else {
        failed_attempts <- failed_attempts + 1
      }
    }
    
    if (failed_attempts == max_attempts) {
      print_simulation_parms(sim_params)
      stop(paste("attempted", max_attempts, "times; something is not right, here's the most recent error:",
                 if (inherits(r, "error")) r$message else r))
    }
  }
  
  # Assign iteration number
  if (is.data.frame(r)) {
    r$iter <- iter
  } else if (is.list(r)) {
    if (!all(sapply(r, is.data.frame))) {
      print(if (inherits(r, "error")) r$message else r)
      stop(paste0("Error in run_iteration. Results are not data.frames. Error: \n", if (inherits(r, "error")) r$message else r))
    }
    r <- lapply(r, function(x) { x$iter <- iter; x })
  }
  
  return(r)
}
