### new_sim() ###

# new_sim() creates a simulation object
sim <- new_sim()

test_that("new_sim() creates correctly-specified object", {
  expect_equal(sim$config$num_sim, 1000)
  expect_equal(sim$config$parallel, "none")
  expect_equal(sim$levels, list("no levels"=TRUE))
  expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$errors, "Simulation has not been run yet.")
  expect_equal(sim$warnings, "Simulation has not been run yet.")
})

# new_sim() throws an error if dependencies are missing
# !!!!! as with the config package test below, I would like a different way of doing this...
#library('magrittr')
#detach('package:magrittr', unload = TRUE)
#test_that("new_sim() with missing dependencies throws error", {
#  expect_error(new_sim(), "You need to install the package 'magrittr' for simba to work.")
#})

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

test_that("add_creator() throws an error if no function is provided", {
  expect_error(add_creator(sim), "You must provide a function to add_creator.")
})

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

test_that("add_method() throws an error if no function is provided", {
  expect_error(add_method(sim), "You must provide a function to add_method.")
})

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

### set_script ###
my_script <- function() {
  df <- create_rct_data(L$num_patients)
  estimate <- do.call(L$estimator, list(df))
  return (
    list("estimate" = estimate)
  )
}

# !!!! how to check the output?
sim <- new_sim()
sim %<>% set_script(my_script)
test_that("set_script() works with predefined function", {
  expect_type(sim$script, "closure")
  expect_equal("..script" %in% ls(sim$internals$env, all.names=TRUE), TRUE)
})

sim <- new_sim()
sim %<>% set_script(
  function() {
    df <- create_rct_data(L$num_patients)
    estimate <- do.call(L$estimator, list(df))
    return (
      list("estimate" = estimate)
    )
  }
)
test_that("set_script() works with function defined in call", {
  expect_type(sim$script, "closure")
  expect_equal("..script" %in% ls(sim$internals$env, all.names=TRUE), TRUE)
})


sim <- new_sim()
test_that("set_script() throws error for non-function second argument", {
  expect_error(set_script(sim, 2), "`fn` must be a function")
})

sim <- new_sim()
sim$internals$run_state <- "run, no errors"
test_that("set_script() throws error if sim has already been run", {
  expect_error(set_script(sim, my_script), "A simulation object's script cannot be changed after the simulation has been run.")
})

### add_constants() ###

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

test_that("set_levels() throws error if levels are not a list of key-value pairs", {
  expect_error(set_levels(sim, estimator = c("estimator_1", "estimator_2"), c(50, 100)),
               "Simulation levels must be a list of key-value pairs." )
})

sim <- new_sim()
sim %<>% set_levels(
  num_patients = c(50, 100)
)
sim %<>% set_levels(
  num_patients = c(200),
  .add = TRUE
)
test_that(".add argument works to add new levels to existing", {
  expect_equal(sim$levels$num_patients, c(50, 100, 200))
})

### set_config() ###

# no arguments supplied
test_that("set_config() throws error with no config provided", {
  expect_error(set_config(sim), "No configuration options were specified")
})

# invalid config option name
test_that("set_config() throws error for invalid config option name", {
  expect_error(set_config(sim, fake_name = "foobar"),
               "unused argument \\(fake_name = \"foobar\"\\)")
})

# invalid config values
test_that("set_config() throws errors for invalid config option values", {
  expect_error(set_config(sim, num_sim="hey"), "`num_sim` must be numeric")
  expect_error(set_config(sim, datasets="hey"),
               "'hey' is not a valid option for `datasets`")
  expect_error(set_config(sim, parallel=c("inner","outer")),
               "`parallel` cannot be a vector")
  expect_error(set_config(sim, packages=min),
               "`packages` must be a character vector")
  expect_error(set_config(sim, stop_at_error=1),
               "`stop_at_error` must be of type 'logical'")
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
sim %<>% set_script(
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
sim %<>% run()


### seeds ###

sim <- new_sim()
sim %<>% set_script(function() {
  return(list(x=rnorm(1)))
})
sim %<>% set_config(num_sim=3)
res1 <- round(run(sim)$results$x, 4)
res2 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=1)
res3 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=2)
res4 <- round(run(sim)$results$x, 4)
res5 <- round(run(sim)$results$x, 4)

test_that("setting seeds leads to reproducible results (parallel='none')", {
  expect_equal(res1, res2)
  expect_equal(res1, res3)
  expect_equal(isTRUE(all.equal(res1, res4)), FALSE)
  expect_equal(res4, res5)
})

sim <- new_sim()
sim %<>% set_script(function() {
  return(list(x=rnorm(1)))
})
sim %<>% set_config(num_sim=3, parallel="outer")
res6 <- round(run(sim)$results$x, 4)
res7 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=1)
res8 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=2)
res9 <- round(run(sim)$results$x, 4)
res10 <- round(run(sim)$results$x, 4)

test_that("setting seeds leads to reproducible results (parallel='outer')", {
  expect_equal(res6, res7)
  expect_equal(res6, res8)
  expect_equal(isTRUE(all.equal(res6, res9)), FALSE)
  expect_equal(res9, res10)
  expect_equal(res1, res6)
  expect_equal(res4, res9)
})

### complex simulation return data ###
sim_1 <- new_sim()
sim_2 <- new_sim()
sim_3 <- new_sim()
sim_1 %<>% set_levels(n=c(4,9))
sim_2 %<>% set_levels(n=c(4,9))
sim_3 %<>% set_levels(n=c(4,9))
sim_1 %<>% set_config(num_sim=1)
sim_2 %<>% set_config(num_sim=1)
sim_3 %<>% set_config(num_sim=1)
sim_1 %<>% set_script(function() {
  dat <- c(1:(L$n))
  mtx <- matrix(dat, nrow=sqrt(length(dat)))
  return (list(
    "mean" = mean(dat),
    "det" = det(mtx),
    ".complex" = list(dat=dat, mtx=mtx)
  ))
})
sim_2 %<>% set_script(function() {
  dat <- c(1:(L$n))
  mtx <- matrix(dat, nrow=sqrt(length(dat)))
  return (list(
    ".complex" = list(dat=dat, mtx=mtx)
  ))
})
sim_3 %<>% set_script(function() {
  dat <- c(1:(L$n))
  mtx <- matrix(dat, nrow=sqrt(length(dat)))
  return (list(
    "mean" = mean(dat),
    "det" = det(mtx)
  ))
})
sim_1 %<>% run()
sim_2 %<>% run()
sim_3 %<>% run()

test_that("sim_1 has both non-complex and complex data", {
  expect_equal(sim_1$results[1,"mean"], 2.5)
  expect_equal(sim_1$results[2,"mean"], 5.0)
  expect_equal(sim_1$results_complex[[1]]$sim_uid, 1)
  expect_equal(sim_1$results_complex[[1]]$dat, c(1,2,3,4))
  expect_equal(sim_1$results_complex[[1]]$mtx, matrix(c(1,2,3,4), nrow=2))
})

test_that("sim_2 has only complex data", {
  expect_null(sim_2$results[1,"mean"])
  expect_null(sim_2$results[2,"mean"])
  expect_equal(sim_2$results_complex[[1]]$sim_uid, 1)
  expect_equal(sim_2$results_complex[[1]]$dat, c(1,2,3,4))
  expect_equal(sim_2$results_complex[[1]]$mtx, matrix(c(1,2,3,4), nrow=2))
})

test_that("sim_3 has only non-complex data", {
  expect_equal(sim_3$results[1,"mean"], 2.5)
  expect_equal(sim_3$results[2,"mean"], 5.0)
  expect_equal(sim_3$results_complex, NA)
})

### get() ###

# !!!!! Add tests once get() is replaced
