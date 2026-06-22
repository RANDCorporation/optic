#------------------------------------------------------------------------------#
# Tests for NA-input validation in optic_simulation()
#------------------------------------------------------------------------------#

make_clean_optic_panel <- function(n_units = 6, years = 2010:2018, seed = 1) {
  set.seed(seed)
  dat <- expand.grid(
    state = sprintf("State%02d", seq_len(n_units)),
    year = years,
    stringsAsFactors = FALSE
  )
  dat <- dat[order(dat$state, dat$year), ]
  dat$crude.rate <- stats::rnorm(nrow(dat), mean = 12, sd = 3)
  dat$population <- 1000
  dat$unemploymentrate <- stats::rnorm(nrow(dat), mean = 5, sd = 1)
  dat
}

reg_model <- function(formula = crude.rate ~ treatment_level + unemploymentrate) {
  optic_model(
    name = "test_model",
    type = "reg",
    call = "lm",
    formula = formula,
    se_adjust = "none"
  )
}

base_sim_args <- function(x, models) {
  list(
    x = x,
    models = if (is.list(models) && !inherits(models[[1]], "list")) models else models,
    iters = 2,
    method = "no_confounding",
    unit_var = "state",
    treat_var = "state",
    time_var = "year",
    effect_magnitude = list(0),
    n_units = 2,
    effect_direction = "pos",
    policy_speed = "instant",
    n_implementation_periods = 1,
    verbose = FALSE
  )
}

test_that("optic_simulation errors when outcome has NAs", {
  dat <- make_clean_optic_panel()
  dat$crude.rate[dat$year == 2014] <- NA_real_

  args <- base_sim_args(dat, list(reg_model()))
  expect_error(
    do.call(optic_simulation, args),
    "Input data passed to optic_simulation\\(\\) contains NAs"
  )
})

test_that("optic_simulation NA error names the offending column", {
  dat <- make_clean_optic_panel()
  dat$crude.rate[dat$year == 2014] <- NA_real_

  args <- base_sim_args(dat, list(reg_model()))
  err <- tryCatch(do.call(optic_simulation, args),
                  error = function(e) conditionMessage(e))

  expect_match(err, "crude.rate:.*6 NA values")
  expect_match(err, "state=State", fixed = FALSE)
  expect_match(err, "year=2014", fixed = TRUE)
})

test_that("optic_simulation errors when unit_var has NAs", {
  dat <- make_clean_optic_panel()
  dat$state[3] <- NA_character_

  args <- base_sim_args(dat, list(reg_model()))
  expect_error(
    do.call(optic_simulation, args),
    "state"
  )
})

test_that("optic_simulation errors when time_var has NAs", {
  dat <- make_clean_optic_panel()
  dat$year[5] <- NA_integer_

  args <- base_sim_args(dat, list(reg_model()))
  expect_error(
    do.call(optic_simulation, args),
    "year"
  )
})

test_that("optic_simulation errors when a covariate has NAs", {
  dat <- make_clean_optic_panel()
  dat$unemploymentrate[c(2, 4, 6)] <- NA_real_

  args <- base_sim_args(dat, list(reg_model()))
  expect_error(
    do.call(optic_simulation, args),
    "unemploymentrate"
  )
})

test_that("optic_simulation does not flag unused columns", {
  dat <- make_clean_optic_panel()
  dat$irrelevant_col <- NA_real_

  args <- base_sim_args(
    dat,
    list(reg_model(formula = crude.rate ~ treatment_level + unemploymentrate))
  )

  expect_s3_class(do.call(optic_simulation, args), "OpticSim")
})

test_that("optic_simulation accepts a clean panel", {
  dat <- make_clean_optic_panel()

  args <- base_sim_args(dat, list(reg_model()))
  expect_s3_class(do.call(optic_simulation, args), "OpticSim")
})

test_that("optic_simulation reports columns from the union across models", {
  dat <- make_clean_optic_panel()
  dat$population[1:5] <- NA_real_

  m1 <- reg_model(formula = crude.rate ~ treatment_level + unemploymentrate)
  m2 <- reg_model(formula = crude.rate ~ treatment_level + population)
  m2$name <- "test_model2"

  args <- base_sim_args(dat, list(m1, m2))
  err <- tryCatch(do.call(optic_simulation, args),
                  error = function(e) conditionMessage(e))

  expect_match(err, "population")
})
