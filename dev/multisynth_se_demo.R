# Standalone demonstration that augsynth::multisynth's bootstrap-based
# inference loses power at small N treated, holding the true effect fixed.
# Calls augsynth directly — no optic harness — so the result is purely
# about augsynth.
#
# Run from the optic-core root:
#   Rscript dev/multisynth_se_demo.R

suppressMessages({
  library(optic)         # only for the overdoses dataset
  library(augsynth)
  library(dplyr)
  library(future.apply)
})

data(overdoses)
sim_data <- overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas",
                        "Mississippi", "Oregon", "South Dakota",
                        "North Dakota")))

outcome_mean <- mean(sim_data$crude.rate, na.rm = TRUE)
all_states   <- unique(sim_data$state)

effect_pct <- 0.30
n_treated  <- c(5, 25)
iters      <- 200
treat_year <- 2010

scenarios <- expand.grid(n_trt = n_treated, iter = 1:iters,
                         KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

run_one_scenario <- function(n_trt, iter, panel, all_states, outcome_mean) {
  treated_states <- sample(all_states, n_trt)
  treated_panel <- panel
  treated_panel$treatment_level <- as.integer(
    treated_panel$state %in% treated_states & treated_panel$year >= treat_year
  )
  treated_panel$crude.rate <- treated_panel$crude.rate -
    treated_panel$treatment_level * effect_pct * outcome_mean

  multisynth_fit <- augsynth::multisynth(
    crude.rate ~ treatment_level,
    unit = state, time = year, data = treated_panel, lambda = 1e-6
  )
  bootstrap_summary <- suppressWarnings(
    summary(multisynth_fit, inf_type = "bootstrap")
  )
  avg_att_row <- bootstrap_summary$att[
    bootstrap_summary$att$Level == "Average" & is.na(bootstrap_summary$att$Time),
  ]

  data.frame(
    n_trt    = n_trt,
    iter     = iter,
    estimate = avg_att_row$Estimate,
    se       = avg_att_row$Std.Error,
    ci_lo    = avg_att_row$lower_bound,
    ci_hi    = avg_att_row$upper_bound,
    stringsAsFactors = FALSE
  )
}

cat("Running", nrow(scenarios), "fits on 15 workers (effect = 30% reduction)...\n")
set.seed(47201)
plan(multisession, workers = 15)
start_time <- Sys.time()
results_list <- future_lapply(seq_len(nrow(scenarios)), function(idx) {
  run_one_scenario(scenarios$n_trt[idx], scenarios$iter[idx], sim_data, all_states, outcome_mean)
}, future.seed = TRUE)
plan(sequential)
cat("Elapsed:", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "min\n")

results <- do.call(rbind, results_list)
saveRDS(results, "dev/multisynth_se_demo_results.rds")
cat("Saved", nrow(results), "rows to dev/multisynth_se_demo_results.rds\n\n")

summ <- results %>%
  group_by(n_trt) %>%
  summarize(
    n_fits      = n(),
    true_effect = -effect_pct * outcome_mean,
    mean_est    = mean(estimate),
    sd_est      = sd(estimate),
    mean_se     = mean(se),
    rejection   = mean(ci_lo > 0 | ci_hi < 0),
    .groups     = "drop"
  )

cat("--- Summary ---\n")
print(summ %>% mutate(across(where(is.numeric), ~round(.x, 3))), n = Inf)
saveRDS(summ, "dev/multisynth_se_demo_summary.rds")
cat("\nSaved summary to dev/multisynth_se_demo_summary.rds\n")
