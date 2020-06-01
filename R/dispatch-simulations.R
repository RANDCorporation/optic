#' Execute simulations defined in a SimConfig object
#' 
#' @param sim_config SimConfig object created using 'create_config'
#' @param use_future default FALSE, set to TRUE if you have already setup a future
#'     plan (e.g., multiprocess, cluster, etc) and would like for the iterations to
#'     be run in parallel
#' @param seed if not null, will use the index of the simulation * seed provided for
#'     each indepdendent set of simulations
#' @param verbose default TRUE, have the dispatcher tell you what's currently running
#' 
#' @export
dispatch_simulations <- function(sim_config, use_future=FALSE, seed=NULL, verbose=TRUE) {
  #TODO: write out as you go? catch in case of error to not have to restart; return
  #      fully completed sims or return even partial results.
  return_list <- list()
  
  # iterate over all combinations in config
  for (i in 1:nrow(sim_config$combination_args)) {
    single_simulation <- sim_config$setup_single_simulation(i)
    if (verbose) {
      message("JOB DISPATCHED:")
      message(paste("        Iterations:", single_simulation$iters))
      message(paste("        Model Call:", single_simulation$model_call))
      message(paste("        N Treated Units:", single_simulation$n_units))
      message(paste("        Effect Magnitude:", paste(single_simulation$effect_magnitude, collapse=", ")))
      message(paste("        Effect Direction:", single_simulation$effect_direction))
      message(paste("        Policy Implementation Speed:", single_simulation$policy_speed))
      message(paste("        Change Code Treatment:", single_simulation$change_code_treatment))
      if (!is.null(single_simulation$rhos)) {
        message(paste("        Concurrent, rho:", single_simulation$rhos))
      }
      if (use_future) {
        message("        Dispatch Method: parallel (future)")
      } else {
        message("        Dispatch Method: single-threaded")
      }
      message(paste("        Model Specification:", Reduce(paste, trimws(deparse(single_simulation$model_formula)))))
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
            if (class(r) == "data.frame") {
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
          r$iter <- j
          return(r)
        },
        future.seed=use_seed
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
        complete <- 0
        while (complete < 1) {
          r <- tryCatch({
            run_iteration(single_simulation)
          },
          error=function(e) {
            e
          })
          if (class(r) == "data.frame") {
            complete <- complete + 1
          }
        }
        r$iter <- j
        sim_results[[j]] <- r
        print(r)
        rm(r)
        cat("\n")
      }
    }
    
    #==========================================================================
    #==========================================================================
    # CLEAN UP AND ADD META DATA TO RUN
    #==========================================================================
    #==========================================================================
    full_results <- do.call("rbind", sim_results)
    
    # add in config/meta information to the run
    full_results$model_call <- single_simulation$model_call
    full_results$model_formula <- Reduce(paste, trimws(deparse(single_simulation$model_formula)))
    full_results$n_units <- single_simulation$n_units
    full_results$true_effect <- paste(single_simulation$effect_magnitude, collapse=", ")
    full_results$effect_direction <- single_simulation$effect_direction
    full_results$policy_speed <- single_simulation$policy_speed
    full_results$change_code_treatment <- single_simulation$change_code_treatment
    if (!is.null(single_simulation$rhos)) {
      full_results$rho <- single_simulation$rhos
    }
    if (!is.null(seed)) {
      full_results$seed <- use_seed
    }
    
    return_list[[i]] <- full_results
  }
  
  return(return_list)
}