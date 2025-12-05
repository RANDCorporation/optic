# Parse a formula object into its left-hand-side and right-hand-side components

Parse a formula object into its left-hand-side and right-hand-side
components

## Usage

``` r
model_terms(x)
```

## Arguments

- x:

  Formula to parse

## Value

list with named elements "lhs" and "rhs", containing variables on each
respective side of the equation

## Examples

``` r
# Set up a hypothetical function, then decompose into left-hand and 
# right-hand sides
form <- formula(outcome ~ treatment + confounder + unit + time)
model_terms(form)
#> $lhs
#> [1] "outcome"
#> 
#> $rhs
#> [1] "treatment"  "confounder" "unit"       "time"      
#> 
```
