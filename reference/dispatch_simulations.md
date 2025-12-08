# Dispatch Simulations

Runs optic simulations

## Usage

``` r
dispatch_simulations(
  object,
  seed = NULL,
  use_future = FALSE,
  verbose = 0,
  graceful = FALSE,
  ...
)
```

## Arguments

- object:

  Simulation scenarios object created using \`optic_simulation\`

- seed:

  Specified as either NULL or a numeric. Sets a seed, which is becomes
  an index in results, for each independent set of simulations in
  \`optic_simulation\`.

- use_future:

  Runs simulation scenarios in parallel. Default FALSE, set to TRUE if
  you would like for the iterations to be run in parallel.

- verbose:

  Default 0. If greater than 0, provides details on what is currently
  running.

- graceful:

  If TRUE, errors in iterations are caught and retried up to 10 percent
  of total iterations. If FALSE, errors are not caught and will stop
  execution. Default is FALSE.

- ...:

  Additional parameters to be passed to model call.

## Value

A single \`data.frame\` containing estimated treatment effects and
summary statistics by model and draw, bound together to allow for
different column names.
