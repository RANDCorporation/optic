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
dispatch_simulations <- function(sim_config, use_future=FALSE, seed=NULL, failure=NULL, verbose=0, ...) {
  if (!"SimConfig" %in% class(sim_config)) {
    stop("`sim_config` must be a SimConfig object")
  }
  
  return_list <- list()
  
  # iterate over all combinations in config
  for (i in 1:nrow(sim_config$simulation_params)) {
    single_simulation <- sim_config$setup_single_simulation(i)
    if (verbose > 0) {
      cat(paste("JOB", i, "OF", nrow(sim_config$simulation_params), "DISPATCHED:\n"))
      if (use_future) {
        cat("        DISPATCH METHOD: parallel (future)\n")
      } else {
        cat("        DISPATCH METHOD: single-threaded\n")
      }
      if (verbose > 1) {
        cat("        PARAMS:\n")
        for (p in names(sim_config$simulation_params[i, ])) {
          cat(paste0("            ", p, ": ", sim_config$simulation_params[i, p], "\n"))
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
          r$iter <- j
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
          }
          failed_attempts <- failed_attempts + 1
        }
        if (failed_attempts == 0.10*single_simulation$iters) {
          stop(paste("attempted 10 percent of total iterations in single thread;",
                     "something is not right, here's the most recent error:",
                     r))
        }
        r$iter <- j
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