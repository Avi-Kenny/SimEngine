
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

### min errors
test_that("Invalid or missing arguments to min throw errors", {
  expect_error(summary(sim, min = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, min = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, min = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, min = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, min = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
})

### max errors
test_that("Invalid or missing arguments to max throw errors", {
  expect_error(summary(sim, max = list(x = "x")),
               "`name` argument is required.") # no name
  expect_error(summary(sim, max = list(name = "my_summary")),
               "`x` argument is required.") # no x
  expect_error(summary(sim, max = list(name = 7, x = "x")),
               "`name` must be a character string.") # non-character name
  expect_error(summary(sim, max = list(name = "my_summary", x = "x1")),
               "x1 is not a variable in results.") # x is not in results
  expect_error(summary(sim, max = list(name = "my_summary", x = "y")),
               "y is not numeric.") # x is not numeric
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
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", upper = "x1", truth = 7)),
               "x1 is not a variable in results.") # upper is not in results
  expect_error(summary(sim, coverage = list(name = "my_summary", lower = "x", upper = "y", truth = 7)),
               "y is not numeric.") # upper is not numeric
})


### proper functioning of mean summary

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    return (list("x"=c(1,2,3,4,5, NA),
                 "y" = c(6, 7, 8, 9, 10, 11)))
  }
)

sim %<>% run("my script")

summ <- sim %>% summary(
  mean = list(name="my_summary", x="x")
)

test_that("mean summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summary(
  mean = list(name="my_summary", x="x", na.rm=TRUE)
)

test_that("mean summary with na.rm returns mean", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5)))
})

summ <- sim %>% summary(
  mean = list(list(name="my_summary", x="x", na.rm=TRUE),
              list(name = "my_summary2", x="y"))
)

test_that("mean summary of two variables returns both means", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mean(c(1,2,3,4,5)))
  expect_equal(summ$my_summary2, mean(c(6,7,8,9,10,11)))
})


### proper functioning of sd summary

summ <- sim %>% summary(
  sd = list(name="my_summary", x="x")
)

test_that("sd summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summary(
  sd = list(name="my_summary", x="x", na.rm=TRUE)
)

test_that("sd summary with na.rm returns sd", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_equal(summ$my_summary, sd(c(1,2,3,4,5)))
})

summ <- sim %>% summary(
  sd = list(list(name="my_summary", x="x", na.rm=TRUE),
              list(name = "my_summary2", x="y"))
)

test_that("sd summary of two variables returns both sds", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, sd(c(1,2,3,4,5)))
  expect_equal(summ$my_summary2, sd(c(6,7,8,9,10,11)))
})

### proper functioning of var summary

summ <- sim %>% summary(
  var = list(name="my_summary", x="x")
)

test_that("var summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summary(
  var = list(name="my_summary", x="x", na.rm=TRUE)
)

test_that("var summary with na.rm returns var", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_equal(summ$my_summary, var(c(1,2,3,4,5)))
})

summ <- sim %>% summary(
  var = list(list(name="my_summary", x="x", na.rm=TRUE),
            list(name = "my_summary2", x="y"))
)

test_that("var summary of two variables returns both vars", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, var(c(1,2,3,4,5)))
  expect_equal(summ$my_summary2, var(c(6,7,8,9,10,11)))
})

### proper functioning of mad summary

summ <- sim %>% summary(
  mad = list(name="my_summary", x="x")
)

test_that("mad summary without na.rm returns NA", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_true(is.na(summ$my_summary))
})

summ <- sim %>% summary(
  mad = list(name="my_summary", x="x", na.rm=TRUE)
)

test_that("mad summary with na.rm returns mad", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 2))
  expect_equal(summ$my_summary, mad(c(1,2,3,4,5)))
})

summ <- sim %>% summary(
  mad = list(list(name="my_summary", x="x", na.rm=TRUE),
             list(name = "my_summary2", x="y"))
)

test_that("mad summary of two variables returns both mads", {
  expect_type(summ, "list")
  expect_equal(dim(summ), c(1, 3))
  expect_equal(summ$my_summary, mad(c(1,2,3,4,5)))
  expect_equal(summ$my_summary2, mad(c(6,7,8,9,10,11)))
})

### proper functioning of iqr summary

# summ <- sim %>% summary(
#   iqr = list(name="my_summary", x="x")
# )
#
# test_that("iqr summary without na.rm returns NA", {
#   expect_type(summ, "list")
#   expect_equal(dim(summ), c(1, 2))
#   expect_true(is.na(summ$my_summary))
# })
#
# summ <- sim %>% summary(
#   iqr = list(name="my_summary", x="x", na.rm=TRUE)
# )
#
# test_that("iqr summary with na.rm returns iqr", {
#   expect_type(summ, "list")
#   expect_equal(dim(summ), c(1, 2))
#   expect_equal(summ$my_summary, IQR(c(1,2,3,4,5)))
# })
#
# summ <- sim %>% summary(
#   iqr = list(list(name="my_summary", x="x", na.rm=TRUE),
#              list(name = "my_summary2", x="y"))
# )
#
# test_that("iqr summary of two variables returns both iqrs", {
#   expect_type(summ, "list")
#   expect_equal(dim(summ), c(1, 3))
#   expect_equal(summ$my_summary, IQR(c(1,2,3,4,5)))
#   expect_equal(summ$my_summary2, IQR(c(6,7,8,9,10,11)))
# })

