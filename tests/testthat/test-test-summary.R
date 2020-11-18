
### Blank summary

sim <- new_sim()

test_that("Summary of blank sim object throws an error", {
  expect_error(summary(sim), "Simulation has not been run yet.")
})



### Summary of object with 100% errors

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    x <- matrix(c(1,1,1,1), nrow=2)
    x <- solve(x)
    return (list("x"=1))
  }
)

sim %<>% set_config(
  num_sim = 10
)

sim %<>% run("my script")

test_that("Summary of blank sim object throws an error", {
  expect_error(summary(sim), "100% of simulations had errors.")
})

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    return (list("x"=c(1,2,3,4,5),
                 "y" = c("a", "b", "c", "d", "e")))
  }
)

sim %<>% run("my script")

### invalid metric
test_that("Invalid metric throws error", {
  expect_error(summary(sim, cov = list(estimate = "x", se = "x")),
               "cov is an invalid summary metric.")
})

### mean errors
test_that("Invalid or missing arguments to mean throw errors", {
  expect_error(summary(sim, mean = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, mean = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, mean = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, mean = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, mean = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### standard deviation errors
test_that("Invalid or missing arguments to sd throw errors", {
  expect_error(summary(sim, sd = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, sd = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, sd = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, sd = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, sd = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### variance errors
test_that("Invalid or missing arguments to var throw errors", {
  expect_error(summary(sim, var = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, var = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, var = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, var = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, var = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### mad errors
test_that("Invalid or missing arguments to mad throw errors", {
  expect_error(summary(sim, mad = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, mad = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, mad = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, mad = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, mad = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### iqr errors
test_that("Invalid or missing arguments to iqr throw errors", {
  expect_error(summary(sim, iqr = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, iqr = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, iqr = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, iqr = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, iqr = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### quantile errors
test_that("Invalid or missing arguments to quantile throw errors", {
  expect_error(summary(sim, quantile = list(x = "x", prob = 0.4)),
               "`name` argument is required.") # no name
  expect_error(summary(sim, quantile = list(name = "my_summary", prob = 0.4)),
               "`x` argument is required.") # no x
  expect_error(summary(sim, quantile = list(name = "my_summary", x = "x")),
               "`prob` argument is required.") # no prob
  expect_error(summary(sim, quantile = list(name = 7, x = "x", prob = 0.5)),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, quantile = list(name = "my_summary", x = "x1", prob = 0.5)),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, quantile = list(name = "my_summary", x = "y", prob = 0.5)),
               "y is not numeric.") # x is not numeric
  expect_error(summary(sim, quantile = list(name = "my_summary", x = "x", prob = "c")),
               "c is not numeric.") # prob is not numeric
  expect_error(summary(sim, quantile = list(name = "my_summary", x = "x", prob = 1.1)),
               "1.1 is not a number between 0 and 1.") # prob is outside [0,1]
})

### median errors
test_that("Invalid or missing arguments to median throw errors", {
  expect_error(summary(sim, median = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, median = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, median = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, median = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, median = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### bias errors
test_that("Invalid or missing arguments to bias throw errors", {
  expect_error(summary(sim, bias = list(estimate = "x", truth = 7)),
               "`name` argument is required.") # no name
  expect_error(summary(sim, bias = list(name = "my_summary", truth = 7)),
               "`estimate` argument is required.") # no estimate
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "x")),
               "`truth` argument is required.") # no truth
  expect_error(summary(sim, bias = list(name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "x1", truth = 7)),
               "x1 is not a variable in results.") # estimate is not in results
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "y", truth = 7)),
               "y is not numeric.") # estimate is not numeric
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "x", truth = "x2")),
               "x2 is not a variable in results.") # truth is a variable name, but not in results
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "x", truth = "y")),
               "y is not numeric.") # truth is non-numeric variable
  expect_error(summary(sim, bias = list(name = "my_summary", estimate = "x", truth = TRUE)),
               "TRUE is neither a number nor a variable in results.") # truth is neither a variable name nor a number
})

### mse errors
test_that("Invalid or missing arguments to mse throw errors", {
  expect_error(summary(sim, mse = list(estimate = "x", truth = 7)),
               "`name` argument is required.") # no name
  expect_error(summary(sim, mse = list(name = "my_summary", truth = 7)),
               "`estimate` argument is required.") # no estimate
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "x")),
               "`truth` argument is required.") # no truth
  expect_error(summary(sim, mse = list(name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "x1", truth = 7)),
               "x1 is not a variable in results.") # estimate is not in results
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "y", truth = 7)),
               "y is not numeric.") # estimate is not numeric
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "x", truth = "x2")),
               "x2 is not a variable in results.") # truth is a variable name, but not in results
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "x", truth = "y")),
               "y is not numeric.") # truth is non-numeric variable
  expect_error(summary(sim, mse = list(name = "my_summary", estimate = "x", truth = TRUE)),
               "TRUE is neither a number nor a variable in results.") # truth is neither a variable name nor a number
})

### mae errors
test_that("Invalid or missing arguments to mae throw errors", {
  expect_error(summary(sim, mae = list(estimate = "x", truth = 7)),
               "`name` argument is required.") # no name
  expect_error(summary(sim, mae = list(name = "my_summary", truth = 7)),
               "`estimate` argument is required.") # no estimate
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "x")),
               "`truth` argument is required.") # no truth
  expect_error(summary(sim, mae = list(name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "x1", truth = 7)),
               "x1 is not a variable in results.") # estimate is not in results
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "y", truth = 7)),
               "y is not numeric.") # estimate is not numeric
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "x", truth = "x2")),
               "x2 is not a variable in results.") # truth is a variable name, but not in results
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "x", truth = "y")),
               "y is not numeric.") # truth is non-numeric variable
  expect_error(summary(sim, mae = list(name = "my_summary", estimate = "x", truth = TRUE)),
               "TRUE is neither a number nor a variable in results.") # truth is neither a variable name nor a number
})

### cov errors
test_that("Invalid or missing arguments to cov throw errors", {
  expect_error(summary(sim, coverage = list(estimate = "x", se = "x", truth = 7)),
               "`name` argument is required.") # no name
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "x")),
               "`truth` argument is required.") # no truth
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "x", truth = "x2")),
               "x2 is not a variable in results.") # truth is a variable name, but not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "x", truth = "y")),
               "y is not numeric.") # truth is non-numeric variable
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "x", truth = TRUE)),
               "TRUE is neither a number nor a variable in results.") # truth is neither a variable name nor a number
  expect_error(summary(sim, coverage = list(name = "my_summary", truth = 7)),
               "Either `estimate` and `se` OR `lower` and `upper` must be provided.") # no est/se or lower/upper arguments
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", truth = 7)),
               "Both `lower` and `upper` must be provided.") # no upper to go with lower
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", truth = 7)),
               "Both `estimate` and `se` must be provided.") # no se
  expect_error(summary(sim, coverage = list(name = 7, estimate = "x", se = "x", truth = 7)),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x1", se = "x", truth = 7)),
               "x1 is not a variable in results.") # estimate is not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "y", se = "x", truth = 7)),
               "y is not numeric.") # estimate is not numeric
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "x1", truth = 7)),
               "x1 is not a variable in results.") # se is not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", estimate = "x", se = "y", truth = 7)),
               "y is not numeric.") # se is not numeric
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x1", upper = "x", truth = 7)),
               "x1 is not a variable in results.") # lower is not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "y", upper = "x", truth = 7)),
               "y is not numeric.") # lower is not numeric
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = TRUE, upper = "x", truth = 7)),
               "TRUE is neither a number nor a variable in results.") # lower is neither a variable name nor number
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", upper = "x1", truth = 7)),
               "x1 is not a variable in results.") # upper is not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", upper = "y", truth = 7)),
               "y is not numeric.") # upper is not numeric
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", upper = TRUE, truth = 7)),
               "TRUE is neither a number nor a variable in results.") # upper is neither a variable name nor number
})

#sim %>% summary(
#  bias = list(name=7, truth="ate", estimate="estimate")
#)
