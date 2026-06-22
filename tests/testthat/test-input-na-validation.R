test_that("optic_simulation NA-input validation", {
  set.seed(1)
  base <- expand.grid(state = sprintf("S%02d", 1:6), year = 2010:2018,
                      stringsAsFactors = FALSE)
  base$crude.rate <- stats::rnorm(nrow(base), 12, 3)
  base$population <- 1000
  base$unemploymentrate <- stats::rnorm(nrow(base), 5, 1)
  mk <- function(fml = crude.rate ~ treatment_level + unemploymentrate, name = "m")
    optic_model(name = name, type = "reg", call = "lm", formula = fml, se_adjust = "none")
  run <- function(x, m) optic_simulation(x = x, models = m, iters = 2,
    method = "no_confounding", unit_var = "state", treat_var = "state",
    time_var = "year", effect_magnitude = list(0), n_units = 2,
    effect_direction = "pos", policy_speed = "instant",
    n_implementation_periods = 1, verbose = FALSE)

  d <- base; d$crude.rate[d$year == 2014] <- NA_real_
  err <- tryCatch(run(d, list(mk())), error = conditionMessage)
  expect_match(err, "optic_simulation\\(\\) requires NA-free")
  expect_match(err, "crude.rate:.*6 NA values")
  expect_match(err, "year=2014", fixed = TRUE)

  for (col in c("state", "year", "unemploymentrate")) {
    d <- base; d[[col]][3] <- NA
    expect_error(run(d, list(mk())), col)
  }

  d <- base; d$unused <- NA_real_
  expect_s3_class(run(d, list(mk())), "OpticSim")
  expect_s3_class(run(base, list(mk())), "OpticSim")

  d <- base; d$population[1:5] <- NA_real_
  err <- tryCatch(run(d, list(mk(name = "m1"),
                               mk(crude.rate ~ treatment_level + population, "m2"))),
                  error = conditionMessage)
  expect_match(err, "population")
})
