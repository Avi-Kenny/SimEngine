### new_sim() ###

# !!!!!
ff <- (function() {

  x1 <- 11
  x2 <- 22
  f1 <- function() { return(33) }
  sim <- new_sim()
  sim %<>% set_levels(lev=c(1,2))
  sim %<>% set_script(function() {
    val <- use_method("f1", list())
    return (list("estimate"=val))
  })
  sim %<>% set_config(num_sim=1)
  sim %<>% run()
  # run(sim)
  sim$errors

  test_that("!!!!! TEMP", {
    expect_equal(sim$errors, "No errors")
    # expect_equal(sim$errors[1,"message"], "")
    # expect_equal(sim$errors[1,"call"], "")
  })


})
# print("environment(ff)")
# print(environment(ff))
ff()

# new_sim() creates a simulation object
sim <- new_sim()

test_that("new_sim() creates correctly-specified object", {
  expect_equal(sim$config$num_sim, 1)
  expect_equal(sim$config$parallel, FALSE)
  expect_equal(sim$levels, list("no_levels"=TRUE))
  expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$errors, "Simulation has not been run yet.")
  expect_equal(sim$warnings, "Simulation has not been run yet.")
})

# new_sim() throws an error if dependencies are missing
# !!!!! as with the config package test below, I would like a different way of doing this...
#library('magrittr')
#detach('package:magrittr', unload = TRUE)
#test_that("new_sim() with missing dependencies throws error", {
#  expect_error(new_sim(), "You need to install the package 'magrittr' for SimEngine to work.")
#})

### set_script ###

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

my_script <- function() {
  df <- create_rct_data(L$num_patients)
  estimate <- use_method(L$estimator, list(df))
  return (
    list("estimate" = estimate)
  )
}

# !!!! how to check the output?
sim <- new_sim()
sim %<>% set_script(my_script)
test_that("set_script() works with predefined function", {
  expect_type(sim$script, "closure")
  expect_equal("..script" %in% ls(sim$vars$env, all.names=TRUE), TRUE)
})

sim <- new_sim()
sim %<>% set_script(
  function() {
    df <- create_rct_data(L$num_patients)
    estimate <- use_method(L$estimator, list(df))
    return (
      list("estimate" = estimate)
    )
  }
)
test_that("set_script() works with function defined in call", {
  expect_type(sim$script, "closure")
  expect_equal("..script" %in% ls(sim$vars$env, all.names=TRUE), TRUE)
})


sim <- new_sim()
test_that("set_script() throws error for non-function second argument", {
  expect_error(set_script(sim, 2), "`fn` must be a function")
})

sim <- new_sim()
sim$vars$run_state <- "run, no errors"
test_that("set_script() throws error if sim has already been run", {
  expect_error(set_script(sim, my_script), "A simulation object's script cannot be changed after the simulation has been run.")
})

### set_levels() ###

# non-list levels
sim <- new_sim()
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
sim <- new_sim()
test_that("set_levels() throws an error if list level does not have names", {
  expect_error(set_levels(sim, estimator = list(list("estimator1"), list("estimator2"))),
               "Each item in a list level must have a name.")
  expect_error(set_levels(sim, estimator = list(a = list("estimator1"), list("estimator2"))),
               "Each item in a list level must have a name.")
})
test_that("set_levels() throws an error if list level is not comprised of lists", {
  expect_error(set_levels(sim, estimator = list(a = "estimator1", b = "estimator2")),
               "Each item in a list level must be a list.")
})
sim %<>% set_levels(method = list("estimator_1" = list("estimator_1"),
                                  "estimator_2" = list("estimator_2")))
test_that("set_levels() works with list levels provided", {
  expect_true(sim$internals$levels_types[1])
  expect_equal(sim$internals$levels_shallow[[1]],
               c("estimator_1", "estimator_2"))
})

# no levels supplied
sim <- new_sim()
test_that("set_levels() throws error with no levels provided", {
  expect_error(set_levels(sim), "No levels supplied")
})
test_that("set_levels() throws error if levels are not a list of key-value pairs", {
  expect_error(set_levels(sim, estimator = c("estimator_1", "estimator_2"), c(50, 100)),
               "Simulation levels must be key-value pairs." )
})

# Use of the .keep argument
sim <- new_sim()
sim %<>% set_levels(alpha=c(1,2,3), beta=c(5,6))
sim %<>% set_levels(.keep=c(1,2,6))
test_that(".add argument works to add new levels to existing", {
  expect_equal(sim$levels_grid$level_id, c(1,2,6))
  expect_equal(sim$levels_grid$alpha, c(1,2,3))
  expect_equal(sim$levels_grid$beta, c(5,5,6))
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
  expect_error(set_config(sim, parallel=c(FALSE,TRUE)),
               "`parallel` must be of type 'logical', of length 1")
  expect_error(set_config(sim, packages=min),
               "`packages` must be a character vector")
  expect_error(set_config(sim, stop_at_error=1),
               "`stop_at_error` must be of type 'logical'")
})

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

test_that("setting seeds leads to reproducible results (parallel=FALSE)", {
  expect_equal(res1, res2)
  expect_equal(isTRUE(all.equal(res1, res4)), FALSE)
  expect_equal(res4, res5)
})

sim <- new_sim()
sim %<>% set_script(function() {
  return(list(x=rnorm(1)))
})
sim %<>% set_config(num_sim=3, parallel=TRUE, n_cores=2)
res6 <- round(run(sim)$results$x, 4)
res7 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=1)
res8 <- round(run(sim)$results$x, 4)
sim %<>% set_config(seed=2)
res9 <- round(run(sim)$results$x, 4)
res10 <- round(run(sim)$results$x, 4)

test_that("setting seeds leads to reproducible results (parallel=TRUE)", {
  expect_equal(res6, res7)
  expect_equal(isTRUE(all.equal(res6, res9)), FALSE)
  expect_equal(res9, res10)
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
  expect_equal(sim_1$results_complex[[1]]$dat, c(1,2,3,4))
  expect_equal(sim_1$results_complex[[1]]$mtx, matrix(c(1,2,3,4), nrow=2))
})

test_that("sim_2 has only complex data", {
  expect_null(sim_2$results[1,"mean"])
  expect_null(sim_2$results[2,"mean"])
  expect_equal(sim_2$results_complex[[1]]$dat, c(1,2,3,4))
  expect_equal(sim_2$results_complex[[1]]$mtx, matrix(c(1,2,3,4), nrow=2))
})

test_that("sim_3 has only non-complex data", {
  expect_equal(sim_3$results[1,"mean"], 2.5)
  expect_equal(sim_3$results[2,"mean"], 5.0)
  expect_equal(sim_3$results_complex, list())
})

### Test that use_method can access the proper environment
sim <- new_sim()
estimator_1 <- function() { L$val }
estimator_2 <- function() { L$val }
wrapper_1 <- function() {
  val_hat <- use_method(L$estimator, list())
  return(val_hat)
}
sim %<>% set_levels(
  "val" = 99,
  "estimator" = c("estimator_1", "estimator_2")
)
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  wrapper_2 <- function() { use_method(L$estimator, list()) }
  val_hat_1 <- wrapper_1()
  val_hat_2 <- wrapper_2()
  val_hat_3 <- use_method(L$estimator)
  return (list("v1"=val_hat_1, "v2"=val_hat_2, "v3"=val_hat_3))
})
sim %<>% run()
test_that("use_method can access the proper environment", {
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$results[1,"v1"], 99)
  expect_equal(sim$results[1,"v2"], 99)
  expect_equal(sim$results[1,"v3"], 99)
})

sim <- new_sim()
test_that("vars() works; before run()", {
  expect_equal(sim$vars$num_sim_total, 1)
  expect_equal(sim$vars$run_state, "pre run")
  expect_equal(class(sim$vars$env), "environment")
  expect_null(sim$vars$start_time)
  expect_null(sim$vars$end_time)
  expect_null(sim$vars$total_runtime)
  expect_identical(sim$vars$num_sim_total, vars(sim)$num_sim_total)
  expect_identical(sim$vars$run_state, vars(sim)$run_state)
  expect_identical(sim$vars$env, vars(sim)$env)
  expect_identical(sim$vars$start_time, vars(sim)$start_time)
})
sim %<>% set_script(function() {
  Sys.sleep(0.01)
  return(list(x=1))
})
sim %<>% run()
test_that("vars() works; after run()", {
  expect_equal(sim$vars$run_state, "run, no errors")
  expect_equal(class(sim$vars$start_time)[2], "POSIXt")
  expect_equal(class(sim$vars$end_time)[2], "POSIXt")
  expect_equal(class(sim$vars$total_runtime), "numeric")
  expect_identical(sim$vars$run_state, vars(sim)$run_state)
  expect_identical(sim$vars$start_time, vars(sim)$start_time)
  expect_identical(sim$vars$end_time, vars(sim)$end_time)
  expect_identical(sim$vars$total_runtime, vars(sim)$total_runtime)
})
test_that("vars() handles incorrect variables properly", {
  expect_error(vars(list(x=1), "fake_var"),
               paste0("no applicable method for 'vars' applied to an object of",
                      " class \"list\""))
  expect_error(vars(sim, "fake_var"),
               "'fake_var' is not a valid option for `var`")
  expect_null(sim$vars$fake_var)
})

# Function scoping
{
  run_sim <- function(parallel) {

    sim <- new_sim()
    return_one <- function() { 1 }
    access_level <- function() { L$alpha }
    fn_outer <- function(x) { fn_inner(x) }
    fn_inner <- function(x) { x }
    glb <- list(beta=4)
    access_glb <- function() { glb$beta }
    create_data <- function() {
      w <- return_one()
      x <- fn_outer(fn_inner(return_one()))
      y <- access_level()
      z <- access_glb()
      return (list(w=w,x=x,y=y,z=z))
    }
    sim %<>% set_levels(alpha=c(2,3))
    sim %<>% set_config(num_sim=1, parallel=parallel, n_cores=2)
    sim %<>% set_script(function() {
      d <- create_data()
      return (d)
    })
    sim %<>% run()
    return(sim)
  }

  sim1 <- run_sim(parallel=FALSE)
  sim2 <- run_sim(parallel=TRUE)

  test_that("Simulation ran and all objects were accessible (serial)", {
    expect_equal(sim1$results[1,"w"], 1)
    expect_equal(sim1$results[1,"x"], 1)
    expect_equal(sim1$results[1,"y"], 2)
    expect_equal(sim1$results[2,"y"], 3)
    expect_equal(sim1$results[1,"z"], 4)
  })

  test_that("Simulation ran and all objects were accessible (parallel)", {
    expect_equal(sim2$results[1,"w"], 1)
    expect_equal(sim2$results[1,"x"], 1)
    expect_equal(sim2$results[1,"y"], 2)
    expect_equal(sim2$results[2,"y"], 3)
    expect_equal(sim2$results[1,"z"], 4)
  })
}

# Using use_method() in parallel
sim <- new_sim()
f1 <- function() { 11 }
f2 <- function() { 22 }
sim %<>% set_levels(f=c("f1","f2"))
sim %<>% set_script(function() {
  val <- use_method(L$f, list())
  return(list(val=val))
})
sim %<>% set_config(num_sim=1, parallel=TRUE, n_cores=2)
sim %<>% run()
test_that("Simulation ran and all objects were accessible (serial)", {
  expect_equal(sim$results[1,"val"], 11)
  expect_equal(sim$results[2,"val"], 22)
})

# get_complex
sim <- new_sim()
sim %<>% set_levels(n=c(10, 100))
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() {
  return(list(
    "alpha" = 1,
    ".complex" = list("beta" = matrix(rep(2,4), ncol=2),
                      "gamma" = list("delta"=3))
  ))
})
sim %<>% run()
c1 <- get_complex(sim, sim_uid=1)
test_that("get_complex operates correctly", {
  expect_equal(class(c1$beta)[1], "matrix")
  expect_equal(class(c1$gamma)[1], "list")
  expect_equal(c1$gamma$delta, 3)
  expect_equal(c1$level_id, 1)
  expect_equal(c1$rep_id, 1)
  expect_equal(c1$n, 10)
  expect_error(get_complex(sim, sim_uid=5),
               "There is no result corresponding to sim_uid=5.")
})

# get_complex (no complex return data)
sim <- new_sim()
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() { return(list("alpha"=1)) })
sim %<>% run()
test_that("get_complex returns correct error message", {
  expect_error(get_complex(sim, sim_uid=1),
               "This simulation does not have any complex return data.")
})
