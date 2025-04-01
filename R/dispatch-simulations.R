

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


#' Execute simulations defined in a optic_simulation object
#' 
#' @param object Simulation scenarios object created using optic_simulation
#' @param seed Specified as either NULL or a numeric. Sets a seed, which is becomes an index in results, for
#'     each independent set of simulations in optic_simulation.
#' @param use_future Runs simulation scenarios in parallel. Default FALSE, set to TRUE if you have already setup a future
#'     plan (e.g., multiprocess, cluster, etc) and would like for the iterations to
#'     be run in parallel.
#' @param verbose Default TRUE. IF TRUE, provides details on what's currently running.
#' @param ... additional parameters to be passed to future_apply. User can pass future.globals and future.packages if your code relies on additional packages
#' 
#' @importFrom future.apply future_lapply
#' @importFrom stats simulate
#' @returns A list of dataframes, where each list entry contains results for a set of simulation parameters, with dataframes containing estimated treatment effects and summary statistics by model and  draw.
#' @examples 
#' # Set up a basic model and simulation scenario:
#' data(overdoses)
#' 
#' eff <- 0.1*mean(overdoses$crude.rate, na.rm = TRUE)
#' form <- formula(crude.rate ~ state + year + population + treatment_level)
#' mod <- optic_model(name = 'lin', 
#'                    type = 'reg', 
#'                    call = 'lm', 
#'                    formula = form, 
#'                    se_adjust = 'none')
#' 
#' sim <- optic_simulation(x = overdoses, 
#'                         models = list(mod), 
#'                         method = 'no_confounding', 
#'                         unit_var = 'state', 
#'                         treat_var = 'state',
#'                         time_var = 'year', 
#'                         effect_magnitude = list(eff), 
#'                         n_units = 2, 
#'                         effect_direction = 'pos', 
#'                         iters = 2,
#'                         policy_speed = 'instant', 
#'                         n_implementation_periods = 1)
#' 
#' # Finally, dispatch the simulation:
#' dispatch_simulations(sim)
#' @export
dispatch_simulations <- function(object, seed=NULL, use_future=FALSE, verbose=0, ...) {
  
  # only use this with optic sim objects.
  # This is purposefully not written as a generic
  stopifnot("OpticSim" %in% class(object))
  
  return_list <- list()
  
  # iterate over all combinations in config
  for (i in 1:nrow(object$simulation_params)) {
    single_simulation <- object$setup_single_simulation(i)
    if (verbose > 0) {
      cat(paste("JOB", i, "OF", nrow(object$simulation_params), "DISPATCHED:\n"))
      if (use_future) {
        cat("        DISPATCH METHOD: parallel (future)\n")
      } else {
        cat("        DISPATCH METHOD: single-threaded\n")
      }
      if (verbose > 1) {
        cat("        PARAMS:\n")
        for (p in names(object$simulation_params[i, ])) {
          cat(paste0("            ", p, ": ", object$simulation_params[i, p], "\n"))
        }
      }
    }
    
    # use future if user requests and it's set up
    if (use_future) {
      #==========================================================================
      #==========================================================================
      # PARALLEL DISPATCH ITERATIONS - using future
      #==========================================================================
      #==========================================================================
      if (!is.null(seed)) {
        use_seed <- seed
      } else {
        use_seed <- NULL
      }
      
      # all iterations of this single sim run
      sim_results <- future.apply::future_lapply(
        1:single_simulation$iters,
        FUN=function(j) {
          failed_attempts <- 0
          complete <- 0
          while (complete < 1 & failed_attempts < 0.10 * single_simulation$iters) {
            r <- tryCatch({
              run_iteration(single_simulation)
            },
            error=function(e) {
              e
            })
            if (! "error" %in% class(r)) {
              complete <- complete + 1
            } else {
              if (failed_attempts == 0.10*single_simulation$iters) {
                stop(paste("attempted 10 percent of total iterations in single thread;",
                           "something is not right, here's the most recent error:",
                           r))
              }
              failed_attempts <- failed_attempts + 1
            }
          }
          
          # if this is one data.frame, assign iteration number
          if (is.data.frame(r)) {
            r$iter <- j
            
          # if this is not a data.frame, then it must be a list of data.frames:
          } else {
            # check that it is a list of data.frames:
            stopifnot(is.list(r))
            
            if(!all(sapply(r, is.data.frame))) {
              print(r$message)
              stop(paste0("Error in simulate.sim_config. Results are not data.frames. Error: \n", r$message))
            }
            # stopifnot(all(sapply(r, is.data.frame)))
            r <- lapply(r, function(x){ x$iter <- j})
          }
          return(r)
        },
        future.seed=use_seed,
        ...
      )
    } else {
      #==========================================================================
      #==========================================================================
      # SINGLE THREADED DISPATCH ITERATIONS
      #==========================================================================
      #==========================================================================
      if (!is.null(seed)) {
        use_seed <- seed
        set.seed(use_seed)
      }
      
      sim_results <- list()
      for(j in 1:single_simulation$iters) {
        failed_attempts <- 0
        complete <- 0
        while (complete < 1 & failed_attempts < 0.10 * single_simulation$iters) {
          
          r <- tryCatch({
            run_iteration(single_simulation)
          },
          error=function(e) {
            e
          })
          if (! "error" %in% class(r)) {
            complete <- complete + 1
          } else{
            failed_attempts <- failed_attempts + 1
          }
        }
        if (failed_attempts == 0.10*single_simulation$iters) {
          print(c(i,j))
          stop(paste("attempted 10 percent of total iterations in single thread;",
                     "something is not right, here's the most recent error:",
                     paste(r, collapse=" ")))
          
        }
        
        if (is.data.frame(r)) {
          r$iter <- j
        } else if (is.list(r)) {
          r <- lapply(r, function(x){ x$iter <- j})
        }
        
        sim_results[[j]] <- r
        
        rm(r)
      }
    }
    
    #==========================================================================
    #==========================================================================
    # CLEAN UP AND ADD META DATA TO RUN
    #==========================================================================
    #==========================================================================
    full_results <- do.call("rbind", sim_results)
    
    # add in config/meta information to the run
    if (!is.null(seed)) {
      full_results$seed <- use_seed
    }
    
    return_list[[i]] <- full_results
  }
  
  return(return_list)
}
