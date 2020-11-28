### new_sim() ###
sim <- new_sim()

test_that("new_sim() creates correctly-specified object", {
  expect_equal(sim$config$num_sim, 1000)
  expect_equal(sim$config$parallel, "none")
  expect_equal(sim$levels, list("no levels"=TRUE))
  expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$errors, "Simulation has not been run yet.")
  expect_equal(sim$warnings, "Simulation has not been run yet.")
})



### add_creator() ###

create_rct_data <- function (num_patients) {
  df <- data.frame(
    "patient_id" = integer(),
    "group" = character(),
    "outcome" = double(),
    stringsAsFactors = FALSE
  )
  for (i in 1:num_patients) {
    group <- ifelse(sample(c(0,1), size=1)==1, "treatment", "control")
    treatment_effect <- ifelse(group=="treatment", -7, 0)
    outcome <- rnorm(n=1, mean=130, sd=5) + treatment_effect
    df[i,] <- list(i, group, outcome)
  }
  return (df)
}

# add_creator() works with predefined function
sim %<>% add_creator(create_rct_data)
df <- sim$creators[[1]](5)
test_that("add_creator() works with predefined function", {
  expect_type(sim$creators, "list")
  expect_equal(length(sim$creators), 1)
  expect_type(sim$creators[[1]], "closure")
  expect_equal(df$patient_id, c(1:5))
})

# add_creator() works with function defined in call
sim <- new_sim()
sim %<>% add_creator("create_rct_data2",
                     function (num_patients) {
  df <- data.frame(
    "patient_id" = integer(),
    "group" = character(),
    "outcome" = double(),
    stringsAsFactors = FALSE
  )
  for (i in 1:num_patients) {
    group <- ifelse(sample(c(0,1), size=1)==1, "treatment", "control")
    treatment_effect <- ifelse(group=="treatment", -7, 0)
    outcome <- rnorm(n=1, mean=130, sd=5) + treatment_effect
    df[i,] <- list(i, group, outcome)
  }
  return (df)
})
df <- sim$creators[[1]](5)
test_that("add_creator() works with function defined in call", {
  expect_type(sim$creators, "list")
  expect_equal(length(sim$creators), 1)
  expect_equal(names(sim$creators), c("create_rct_data2"))
  expect_type(sim$creators[[1]], "closure")
  expect_equal(df$patient_id, c(1:5))
})

# add_creator() throws error for non-string name
test_that("add_creator() throws error for non-string name", {
  expect_error(add_creator(sim, 2, function(y){return(y)}),
               "`name` must be a character string")
})

# add_creator() throws error for non-function function
test_that("add_creator() throws error for non-function function", {
  expect_error(add_creator(sim, "func", 2),
               "`fn` must be a function")
})

### add_method() ###

estimator_1 <- function(df) {
  n <- nrow(df)
  true_prob <- 0.5
  sum_t <- sum(df$outcome * (df$group=="treatment"))
  sum_c <- sum(df$outcome * (df$group=="control"))
  return ( sum_t/(n*true_prob) - sum_c/(n*(1-true_prob)) )
}
estimator_2 <- function(df) {
  n <- nrow(df)
  est_prob <- sum(df$group=="treatment") / n
  sum_t <- sum(df$outcome * (df$group=="treatment"))
  sum_c <- sum(df$outcome * (df$group=="control"))
  return ( sum_t/(n*est_prob) - sum_c/(n*(1-est_prob)) )
}

# add_method() works with predefined functions
sim %<>% add_method(estimator_1)
sim %<>% add_method(estimator_2)
result <- sim$methods[[1]](df)
test_that("add_method() works with predefined function", {
  expect_type(sim$methods, "list")
  expect_equal(length(sim$methods), 2)
  expect_equal(names(sim$methods), c("estimator_1", "estimator_2"))
  expect_type(sim$methods[[1]], "closure")
  expect_type(sim$methods[[2]], "closure")
  expect_type(result, "double")
})

# add_method() works with function defined in call
sim <- new_sim()
sim %<>% add_method("estimator_1",
                    function(df) {
                      n <- nrow(df)
                      true_prob <- 0.5
                      sum_t <- sum(df$outcome * (df$group=="treatment"))
                      sum_c <- sum(df$outcome * (df$group=="control"))
                      return ( sum_t/(n*true_prob) - sum_c/(n*(1-true_prob)) )
                    })
result <- sim$methods[[1]](df)
test_that("add_method() works with function defined in call", {
  expect_type(sim$methods, "list")
  expect_equal(length(sim$methods), 1)
  expect_equal(names(sim$methods), c("estimator_1"))
  expect_type(sim$methods[[1]], "closure")
  expect_type(result, "double")
})

# add_method() throws error for non-string name
test_that("add_method() throws error for non-string name", {
  expect_error(add_method(sim, 2, function(y){return(y)}),
               "`name` must be a character string")
})

# add_method() throws error for non-function function
test_that("add_method() throws error for non-function function", {
  expect_error(add_method(sim, "func", 2),
               "`fn` must be a function")
})

### add_script ###
my_script <- function() {
  df <- create_rct_data(L$num_patients)
  estimate <- do.call(L$estimator, list(df))
  return (
    list("estimate" = estimate)
  )
}

# add_script() works with predefined function
# !!!! how to check the output?
sim <- new_sim()
sim %<>% add_script(my_script)
test_that("add_script() works with predefined function", {
  expect_type(sim$scripts, "list")
  expect_equal(length(sim$scripts), 1)
  expect_equal(names(sim$scripts), c("my_script"))
  expect_type(sim$scripts[[1]], "closure")
})

# add_script() works with function defined in call
sim <- new_sim()
sim %<>% add_script(
  "my script",
  function() {
    df <- create_rct_data(L$num_patients)
    estimate <- do.call(L$estimator, list(df))
    return (
      list("estimate" = estimate)
    )
  }
)
test_that("add_script() works with function defined in call", {
  expect_type(sim$scripts, "list")
  expect_equal(length(sim$scripts), 1)
  expect_equal(names(sim$scripts), c("my script"))
  expect_type(sim$scripts[[1]], "closure")
})

# add_script() throws error for non-string name
test_that("add_script() throws error for non-string name", {
  expect_error(add_script(sim, 2, function(y){return(y)}),
               "`name` must be a character string")
})

# add_script() throws error for non-function function
test_that("add_script() throws error for non-function function", {
  expect_error(add_script(sim, "func", 2),
               "`fn` must be a function")
})

### add_constant() ###

sim %<>% add_constants("alpha" = 2)
test_that("add_constant() works", {
  expect_type(sim$constants, "list")
  expect_equal(length(sim$constants), 1)
  expect_equal(names(sim$constants), c("alpha"))
  expect_equal(sim$constants[[1]], 2)
})


### set_levels() ###

# non-list levels
sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 100, 200)
)

test_that("set_levels() works with non-list levels provided", {
  expect_type(sim$levels, "list")
  expect_equal(length(sim$levels), 2)
  expect_equal(sim$levels[[1]], c("estimator_1", "estimator_2"))
  expect_equal(sim$levels[[2]], c(50, 100, 200))
})

# list levels
sim %<>% set_levels(method = list("estimator_1" = "estimator_1",
                                  "estimator_2" = "estimator_2"))
test_that("set_levels() works with list levels provided", {
  expect_true(sim$internals$levels_types[1])
  expect_equal(sim$internals$levels_shallow[[1]],
               c("estimator_1", "estimator_2"))
})

# no levels supplied
test_that("set_levels() throws error with no levels provided", {
  expect_error(set_levels(sim), "No levels supplied")
})


### set_config() ###

# no arguments supplied
test_that("set_config() throws error with no config provided", {
  expect_error(set_config(sim), "No configuration options specified")
})

# invalid config option name
test_that("set_config() throws error for invalid config option name", {
  expect_error(set_config(sim, fake_name = "foobar"),
               "'fake_name' is not a valid configuration option.")
})

# load packages
# !!!! maybe there's a better way to do this? I don't like having to use detach()
# this is especially dumb because I have to load pkg before unloading to make sure
# I have a clean slate
library('ggplot2')
detach('package:ggplot2', unload = TRUE)
sim %<>% set_config(packages = "ggplot2")
loaded <- search()
test_that("set_config() successfully loads an installed package", {
  expect_true("package:ggplot2" %in% loaded)
})


# back to original everything
sim <- new_sim()
sim %<>% add_creator(create_rct_data)
sim %<>% add_method(estimator_1)
sim %<>% add_method(estimator_2)
sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 200, 1000)
)
sim %<>% add_script(
  "my script",
  function() {
    df <- create_rct_data(L$num_patients)
    estimate <- do.call(L$estimator, list(df))
    return (
      list("estimate" = estimate)
    )
  }
)
sim %<>% set_config(
  num_sim = 10,
  parallel = "none"
)
sim %<>% run("my script")

### get() ###

# try to get an invalid variable
test_that("get() throws error for invalid variable name", {
  expect_error(get(sim, "invalid_variable"), "Invalid variable name")
})

# get runtime
runtime <- get(sim, "total_runtime")
test_that("get() returns runtime", {
  expect_equal(length(runtime), 1)
  expect_type(runtime, "double")
})

# get start time
start_time <- get(sim, "start_time")
test_that("get() returns start time", {
  expect_equal(length(start_time), 1)
  expect_equal(class(start_time), c("POSIXct", "POSIXt"))
})

# get end time
end_time <- get(sim, "end_time")
test_that("get() returns end time", {
  expect_equal(length(end_time), 1)
  expect_equal(class(end_time), c("POSIXct", "POSIXt"))
})




#sim %<>% run("my script")

#sim %>% summary()

#sim %>% summary(
#  bias = list(name="bias_ate", truth=-7, estimate="estimate"),
#  mse = list(name="mse_ate", truth=-7, estimate="estimate")
#)
