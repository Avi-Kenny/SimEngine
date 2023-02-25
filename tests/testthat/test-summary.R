
### Blank summary ###

sim <- new_sim()

test_that("Summary of blank sim object throws an error", {
  expect_error(summarize(sim), "Simulation has not been run yet.")
})



### Summary of object with 100% errors ###

sim <- new_sim()

sim %<>% set_script(
  function() {
    x <- matrix(c(1,1,1,1), nrow=2)
    x <- solve(x)
    return (list("x"=1))
  }
)

sim %<>% set_config(
  num_sim = 10
)

sim %<>% run()

test_that("Summary of blank sim object throws an error", {
  expect_error(summarize(sim), "100% of simulations had errors.")
})

sim <- new_sim()

sim %<>% set_script(
  function() {
    return (list("x"=c(1,2,3,4,5),
                 "y" = c("a", "b", "c", "d", "e")))
  }
)

sim %<>% run()

### invalid metric ###
test_that("Invalid metric throws error", {
  expect_error(summarize(sim, list(stat = "cov", estimate = "x", se = "x")),
               "cov is an invalid summary metric.")
})

### no metric ###
test_that("Missing metric throws error", {
  expect_error(summarize(sim, list(estimate = "x", se = "x")),
               "You must provide a type of summary metric.")
})

### mean errors ###
test_that("Invalid or missing arguments to mean throw errors", {
  expect_error(summarize(sim, list(stat = "mean", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "mean", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "mean", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "mean", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### standard deviation errors ###
test_that("Invalid or missing arguments to sd throw errors", {
  expect_error(summarize(sim, list(stat = "sd", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "sd", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "sd", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "sd", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### variance errors ###
test_that("Invalid or missing arguments to var throw errors", {
  expect_error(summarize(sim, list(stat = "var", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "var", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "var", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "var", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### mad errors ###
test_that("Invalid or missing arguments to mad throw errors", {
  expect_error(summarize(sim, list(stat = "mad", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "mad", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "mad", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "mad", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### iqr errors ###
test_that("Invalid or missing arguments to iqr throw errors", {
  expect_error(summarize(sim, list(stat = "iqr", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "iqr", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "iqr", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "iqr", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### quantile errors ###
test_that("Invalid or missing arguments to quantile throw errors", {
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", prob = 0.4)),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", x = "x")),
               "`prob` argument is required") # no prob
  expect_error(summarize(sim, list(stat = "quantile", name = 7, x = "x", prob = 0.5)),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", x = "x1", prob = 0.5)),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", x = "y", prob = 0.5)),
               "`y` must be numeric") # x is not numeric
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", x = "x", prob = "c")),
               "`c` must be numeric") # prob is not numeric
  expect_error(summarize(sim, list(stat = "quantile", name = "my_summary", x = "x", prob = 1.1)),
               "1.1 is not a number between 0 and 1.") # prob is outside [0,1]
})

### min errors ###
test_that("Invalid or missing arguments to min throw errors", {
  expect_error(summarize(sim, list(stat = "min", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "min", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "min", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "min", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### max errors ###
test_that("Invalid or missing arguments to max throw errors", {
  expect_error(summarize(sim, list(stat = "max", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "max", name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "max", name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "max", name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### median errors ###
test_that("Invalid or missing arguments to median throw errors", {
  expect_error(summarize(sim, list(stat = "median", name = "my_summary")),
               "`x` argument is required") # no x
  expect_error(summarize(sim, list(stat = "median",name = 7, x = "x")),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "median",name = "my_summary", x = "x1")),
               "`x1` is not a variable in results") # x is not in results
  expect_error(summarize(sim, list(stat = "median",name = "my_summary", x = "y")),
               "`y` must be numeric") # x is not numeric
})

### bias errors ###
test_that("Invalid or missing arguments to bias throw errors", {
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", truth = 7)),
               "`estimate` argument is required") # no estimate
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "x")),
               "`truth` argument is required") # no truth
  expect_error(summarize(sim, list(stat = "bias", name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "x1", truth = 7)),
               "`x1` is not a variable in results") # estimate is not in results
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "y", truth = 7)),
               "`y` must be numeric") # estimate is not numeric
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "x", truth = "x2")),
               "`x2` is neither a number nor a variable in results") # truth is a variable name, but not in results
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "x", truth = "y")),
               "`y` is neither a number nor a variable in results") # truth is non-numeric variable
  expect_error(summarize(sim, list(stat = "bias", name = "my_summary", estimate = "x", truth = TRUE)),
               "`TRUE` is neither a number nor a variable in results") # truth is neither a variable name nor a number
})

### mse errors ###
test_that("Invalid or missing arguments to mse throw errors", {
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", truth = 7)),
               "`estimate` argument is required") # no estimate
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "x")),
               "`truth` argument is required") # no truth
  expect_error(summarize(sim, list(stat = "mse", name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "x1", truth = 7)),
               "`x1` is not a variable in results") # estimate is not in results
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "y", truth = 7)),
               "`y` must be numeric") # estimate is not numeric
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "x", truth = "x2")),
               "`x2` is neither a number nor a variable in results") # truth is a variable name, but not in results
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "x", truth = "y")),
               "`y` is neither a number nor a variable in results") # truth is non-numeric variable
  expect_error(summarize(sim, list(stat = "mse", name = "my_summary", estimate = "x", truth = TRUE)),
               "`TRUE` is neither a number nor a variable in results") # truth is neither a variable name nor a number
})

### mae errors ###
test_that("Invalid or missing arguments to mae throw errors", {
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", truth = 7)),
               "`estimate` argument is required") # no estimate
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "x")),
               "`truth` argument is required") # no truth
  expect_error(summarize(sim, list(stat = "mae", name = 7, estimate = "x", truth = 7)),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "x1", truth = 7)),
               "`x1` is not a variable in results") # estimate is not in results
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "y", truth = 7)),
               "`y` must be numeric") # estimate is not numeric
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "x", truth = "x2")),
               "`x2` is neither a number nor a variable in results") # truth is a variable name, but not in results
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "x", truth = "y")),
               "`y` is neither a number nor a variable in results") # truth is non-numeric variable
  expect_error(summarize(sim, list(stat = "mae", name = "my_summary", estimate = "x", truth = TRUE)),
               "`TRUE` is neither a number nor a variable in results") # truth is neither a variable name nor a number
})

### cov errors ###
test_that("Invalid or missing arguments to cov throw errors", {
  expect_error(summarize(sim, list(stat = "coverage", estimate = "x", se = "x", truth = 7)),
               "`name` argument is required") # no name
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "x")),
               "`truth` argument is required") # no truth
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "x", truth = "x2")),
               "`x2` is neither a number nor a variable in results") # truth is a variable name, but not in results
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "x", truth = "y")),
               "`y` is neither a number nor a variable in results") # truth is non-numeric variable
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "x", truth = TRUE)),
               "`TRUE` is neither a number nor a variable in results") # truth is neither a variable name nor a number
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", truth = 7)),
               "Either `estimate` and `se` OR `lower` and `upper` must be provided") # no est/se or lower/upper arguments
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", lower = "x", truth = 7)),
               "Either `estimate` and `se` OR `lower` and `upper` must be provided") # no upper to go with lower
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", truth = 7)),
               "Either `estimate` and `se` OR `lower` and `upper` must be provided") # no se
  expect_error(summarize(sim, list(stat = "coverage", name = 7, estimate = "x", se = "x", truth = 7)),
               "`name` must be a character string") # non-character name
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x1", se = "x", truth = 7)),
               "`x1` is not a variable in results") # estimate is not in results
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "y", se = "x", truth = 7)),
               "`y` must be numeric") # estimate is not numeric
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "x1", truth = 7)),
               "`x1` is not a variable in results") # se is not in results
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", estimate = "x", se = "y", truth = 7)),
               "`y` must be numeric") # se is not numeric
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", lower = "x1", upper = "x", truth = 7)),
               "`x1` is not a variable in results") # lower is not in results
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", lower = "y", upper = "x", truth = 7)),
               "`y` must be numeric") # lower is not numeric
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", lower = "x", upper = "x1", truth = 7)),
               "`x1` is not a variable in results") # upper is not in results
  expect_error(summarize(sim, list(stat = "coverage", name = "my_summary", lower = "x", upper = "y", truth = 7)),
               "`y` must be numeric") # upper is not numeric
})


### proper functioning of mean summary ###

sim <- new_sim()

sim %<>% set_script(
  function() {
    return (list("x"=c(1,2,3,4,5, NA),
                 "y" = c(6, 7, 8, 9, 10, 11)))
  }
)

sim %<>% set_config(
  num_sim = 1
)

sim %<>% run()

summ <- sim %>% summarize(
  list(stat = "mean", name="my_summary", x="x")
)

test_that("mean summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "mean", name="my_summary", x="x", na.rm=TRUE)
)

test_that("mean summary with na.rm returns mean", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "mean", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "mean", x="y")
)

test_that("mean summary of two variables returns both means", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5)))
  expect_equal(summ$mean_y, mean(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "mean_y"))
})


### proper functioning of sd summary ###

summ <- sim %>% summarize(
  list(stat = "sd", name="my_summary", x="x")
)

test_that("sd summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "sd", name="my_summary", x="x", na.rm=TRUE)
)

test_that("sd summary with na.rm returns sd", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, sd(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "sd", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "sd", x="y")
)

test_that("sd summary of two variables returns both sds", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, sd(c(1,2,3,4,5)))
  expect_equal(summ$sd_y, sd(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "sd_y"))
})

### proper functioning of var summary ###

summ <- sim %>% summarize(
  list(stat = "var", name="my_summary", x="x")
)

test_that("var summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "var", name="my_summary", x="x", na.rm=TRUE)
)

test_that("var summary with na.rm returns var", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, var(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "var", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "var", x="y")
)

test_that("var summary of two variables returns both vars", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, var(c(1,2,3,4,5)))
  expect_equal(summ$var_y, var(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "var_y"))
})

### proper functioning of covariance summary ###

summ <- sim %>% summarize(
  list(stat = "covariance", name="my_summary", x="x", y="y")
)

test_that("covariance summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "covariance", name="my_summary", x="x", y="y", na.rm=TRUE)
)

test_that("covariance summary with na.rm returns covariance", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, cov(c(1,2,3,4,5), c(6,7,8,9,10)))
})

### proper functioning of correlation summary ###

summ <- sim %>% summarize(
  list(stat = "correlation", name="my_summary", x="x", y="y")
)

test_that("correlation summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "correlation", name="my_summary", x="x", y="y", na.rm=TRUE)
)

test_that("correlation summary with na.rm returns covariance", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, cor(c(1,2,3,4,5), c(6,7,8,9,10)))
})


### proper functioning of mad summary ###

summ <- sim %>% summarize(
  list(stat = "mad", name="my_summary", x="x")
)

test_that("mad summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "mad", name="my_summary", x="x", na.rm=TRUE)
)

test_that("mad summary with na.rm returns mad", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mad(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "mad", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "mad", x="y")
)

test_that("mad summary of two variables returns both mads", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, mad(c(1,2,3,4,5)))
  expect_equal(summ$MAD_y, mad(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "MAD_y"))
})

### proper functioning of iqr summary ###

summ <- sim %>% summarize(
  list(stat = "iqr", name="my_summary", x="x")
)

test_that("iqr summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "iqr", name="my_summary", x="x", na.rm=TRUE)
)

test_that("iqr summary with na.rm returns iqr", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, IQR(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "iqr", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "iqr", x="y")
)

test_that("iqr summary of two variables returns both iqrs", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, IQR(c(1,2,3,4,5)))
  expect_equal(summ$IQR_y, IQR(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "IQR_y"))
})

### proper functioning of quantile summary ###

summ <- sim %>% summarize(
  list(stat = "quantile", name="my_summary", x="x", prob = 0.25)
)

test_that("quantile summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "quantile", name="my_summary", x="x", prob = 0.25, na.rm=TRUE)
)

test_that("quantile summary with na.rm returns quantile", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(as.numeric(summ$my_summary), 2)
})

summ <- sim %>% summarize(
  list(stat = "quantile", name="my_summary", x="x", prob = 0.25, na.rm=TRUE),
  list(stat = "quantile", x="y", prob = 0.75)
)

test_that("quantile summary of two variables returns both quantiles", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(as.numeric(summ$my_summary), 2)
  expect_equal(as.numeric(summ$quantile_0.75_y), 9.75)
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "quantile_0.75_y"))
})

### proper functioning of min summary ###

summ <- sim %>% summarize(
  list(stat = "min", name="my_summary", x="x")
)

test_that("min summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "min", name="my_summary", x="x", na.rm=TRUE)
)

test_that("min summary with na.rm returns min", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, min(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "min", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "min", x="y")
)

test_that("min summary of two variables returns both mins", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, min(c(1,2,3,4,5)))
  expect_equal(summ$min_y, min(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "min_y"))
})

### proper functioning of max summary ###

summ <- sim %>% summarize(
  list(stat = "max", name="my_summary", x="x")
)

test_that("max summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "max", name="my_summary", x="x", na.rm=TRUE)
)

test_that("max summary with na.rm returns max", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, max(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "max", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "max", x="y")
)

test_that("max summary of two variables returns both maxes", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, max(c(1,2,3,4,5)))
  expect_equal(summ$max_y, max(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "max_y"))
})

### proper functioning of median summary ###

summ <- sim %>% summarize(
  list(stat = "median", name="my_summary", x="x")
)

test_that("median summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "median", name="my_summary", x="x", na.rm=TRUE)
)

test_that("median summary with na.rm returns median", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, median(c(1,2,3,4,5)))
})

summ <- sim %>% summarize(
  list(stat = "median", name="my_summary", x="x", na.rm=TRUE),
  list(stat = "median", x="y")
)

test_that("median summary of two variables returns both medians", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, median(c(1,2,3,4,5)))
  expect_equal(summ$median_y, median(c(6,7,8,9,10,11)))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "median_y"))
})

### proper functioning of bias summary ###

summ <- sim %>% summarize(
  list(stat = "bias", name="my_summary", estimate="x", truth = 7)
)

test_that("bias summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "bias", name="my_summary", estimate="x", truth = 7, na.rm=TRUE)
)

test_that("bias summary with na.rm, constant truth, returns bias", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5) - 7))
})

summ <- sim %>% summarize(
  list(stat = "bias", name="my_summary", estimate="x", truth = "y", na.rm=TRUE)
)

test_that("bias summary with na.rm, variable truth, returns bias", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5) - c(6,7,8,9,10)))
})

summ <- sim %>% summarize(
  list(stat = "bias", name="my_summary", estimate="x", truth = 7, na.rm=TRUE),
  list(stat = "bias", estimate="y", truth = 10)
)

test_that("bias summary of two variables, constant truth, returns both biases", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5)-7))
  expect_equal(summ$bias_y, mean(c(6,7,8,9,10,11)-10))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "bias_y"))
})

### proper functioning of mse summary ###

summ <- sim %>% summarize(
  list(stat = "mse", name="my_summary", estimate="x", truth = 7)
)

test_that("mse summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "mse",name="my_summary", estimate="x", truth = 7, na.rm=TRUE)
)

test_that("mse summary with na.rm, constant truth, returns mse", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean((c(1,2,3,4,5) - 7)^2))
})

summ <- sim %>% summarize(
  list(stat = "mse", name="my_summary", estimate="x", truth = "y", na.rm=TRUE)
)

test_that("mse summary with na.rm, variable truth, returns mse", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean((c(1,2,3,4,5) - c(6,7,8,9,10))^2))
})

summ <- sim %>% summarize(
  list(stat = "mse", name="my_summary", estimate="x", truth = 7, na.rm=TRUE),
  list(stat = "mse", estimate="y", truth = 10)
)

test_that("mse summary of two variables, constant truth, returns both mses", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, mean((c(1,2,3,4,5)-7)^2))
  expect_equal(summ$MSE_y, mean((c(6,7,8,9,10,11)-10)^2))
  expect_equal(names(summ), c("level_id", "n_reps", "my_summary", "MSE_y"))
})

### proper functioning of mae summary ###

summ <- sim %>% summarize(
  list(stat = "mae", name="my_summary", estimate="x", truth = 7)
)

test_that("mae summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "mae", name="my_summary", estimate="x", truth = 7, na.rm=TRUE)
)

test_that("mae summary with na.rm, constant truth, returns mae", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(abs(c(1,2,3,4,5) - 7)))
})

summ <- sim %>% summarize(
  list(stat = "mae", name="my_summary", estimate="x", truth = "y", na.rm=TRUE)
)

test_that("mae summary with na.rm, variable truth, returns mae", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(abs(c(1,2,3,4,5) - c(6,7,8,9,10))))
})

summ <- sim %>% summarize(
  list(stat = "mae", name="my_summary", estimate="x", truth = 7, na.rm=TRUE),
  list(stat = "mae", estimate="y", truth = 10)
)

test_that("mae summary of two variables, constant truth, returns both maes", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, mean(abs(c(1,2,3,4,5)-7)))
  expect_equal(summ$MAE_y, mean(abs(c(6,7,8,9,10,11)-10)))
  expect_equal(names(summ), c("level_id",  "n_reps", "my_summary", "MAE_y"))
})

### proper function of CI coverage ###
sim <- new_sim()

sim %<>% set_script(
  function() {
    return (list("x" = c(1,2,3,4,5, NA),
                 "y" = c(6, 7, 8, 9, 10, 11),
                 "z" = c(2, 3, 4, 5, 200, 300)))
  }
)

sim %<>% set_config(num_sim = 1)

sim %<>% run()

summ <- sim %>% summarize(
  list(stat = "coverage", name="my_summary", lower="x", upper="y", truth = 3)
)

test_that("cov summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summarize(
  list(stat = "coverage", name="my_summary", lower="x", upper="y", truth = 3, na.rm=TRUE)
)

test_that("cov summary with na.rm, constant truth, lower and upper, returns cov", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, 0.6)
})

summ <- sim %>% summarize(
  list(stat = "coverage", name="my_summary", lower="x", upper="y", truth = "z", na.rm=TRUE)
)

test_that("cov summary with na.rm, variable truth, lower and upper, returns cov", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, 0.8)
})

summ <- sim %>% summarize(
  list(stat = "coverage", name="my_summary", estimate="x", se="y", truth = "z", na.rm=TRUE)
)

test_that("cov summary with na.rm, variable truth, est and SE, returns cov", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, 0.8)
})

summ <- sim %>% summarize(
  list(stat = "coverage", name="my_summary", lower="x", upper = "y", truth = 3, na.rm=TRUE),
  list(stat = "coverage", name = "my_summary2", lower = "x", upper = "z", truth = 2, na.rm=TRUE)
)

test_that("cov summary of two variables, constant truth, returns both covs", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 4))
  expect_equal(summ$my_summary, 0.6)
  expect_equal(summ$my_summary2, 0.4)
})
