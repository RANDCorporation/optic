# Tests for NA-input validation in optic_simulation()

clean_panel <- function(n = 6, years = 2010:2018, seed = 1) {
  set.seed(seed)
  d <- expand.grid(state = sprintf("S%02d", seq_len(n)),
                   year = years, stringsAsFactors = FALSE)
  d <- d[order(d$state, d$year), ]
  d$crude.rate <- stats::rnorm(nrow(d), 12, 3)
  d$population <- 1000
  d$unemploymentrate <- stats::rnorm(nrow(d), 5, 1)
  d
}

reg_mod <- function(fml = crude.rate ~ treatment_level + unemploymentrate,
                    name = "m") {
  optic_model(name = name, type = "reg", call = "lm",
              formula = fml, se_adjust = "none")
}

run_sim <- function(x, models) {
  optic_simulation(x = x, models = models, iters = 2,
                   method = "no_confounding", unit_var = "state",
                   treat_var = "state", time_var = "year",
                   effect_magnitude = list(0), n_units = 2,
                   effect_direction = "pos", policy_speed = "instant",
                   n_implementation_periods = 1, verbose = FALSE)
}

test_that("flags NA in outcome and names column + example", {
  d <- clean_panel(); d$crude.rate[d$year == 2014] <- NA_real_
  err <- tryCatch(run_sim(d, list(reg_mod())),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Input data passed to optic_simulation\\(\\) contains NAs")
  expect_match(err, "crude.rate:.*6 NA values")
  expect_match(err, "state=S")
  expect_match(err, "year=2014", fixed = TRUE)
})

test_that("flags NA in unit_var, time_var, and covariates", {
  d1 <- clean_panel(); d1$state[3] <- NA_character_
  expect_error(run_sim(d1, list(reg_mod())), "state")
  d2 <- clean_panel(); d2$year[5] <- NA_integer_
  expect_error(run_sim(d2, list(reg_mod())), "year")
  d3 <- clean_panel(); d3$unemploymentrate[c(2, 4, 6)] <- NA_real_
  expect_error(run_sim(d3, list(reg_mod())), "unemploymentrate")
})

test_that("ignores unused columns and accepts a clean panel", {
  d <- clean_panel(); d$unused <- NA_real_
  expect_s3_class(run_sim(d, list(reg_mod())), "OpticSim")
  expect_s3_class(run_sim(clean_panel(), list(reg_mod())), "OpticSim")
})

test_that("checks the union of columns across models", {
  d <- clean_panel(); d$population[1:5] <- NA_real_
  m1 <- reg_mod(name = "m1")
  m2 <- reg_mod(crude.rate ~ treatment_level + population, name = "m2")
  err <- tryCatch(run_sim(d, list(m1, m2)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "population")
})
