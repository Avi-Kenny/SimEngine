### set up sim ###
sim <- new_sim()

create_rct_data <- function (num_patients, ate) {
  df <- data.frame(
    "patient_id" = integer(),
    "group" = character(),
    "outcome" = double(),
    stringsAsFactors = FALSE
  )
  for (i in 1:num_patients) {
    group <- ifelse(sample(c(0,1), size=1)==1, "treatment", "control")
    treatment_effect <- ifelse(group=="treatment", ate, 0)
    outcome <- rnorm(n=1, mean=130, sd=5) + treatment_effect
    df[i,] <- list(i, group, outcome)
  }
  return (df)
}

sim %<>% add_creator(create_rct_data)

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

sim %<>% add_method(estimator_1)

sim %<>% set_levels(
  estimator = c("estimator_1"),
  num_patients = c(50, 100),
  ate = c(-7)
)

sim %<>% set_script(
  function() {
    df <- create_rct_data(L$num_patients, L$ate)
    estimate <- do.call(L$estimator, list(df))
    return (
      list("estimate" = estimate)
    )
  }
)

sim %<>% set_config(
  num_sim = 5
)

sim %<>% run()
prev_ncol <- length(sim$results)
prev_nrow <- nrow(sim$results)
prev_row1 <- sim$results[1,]

### update handles errors ###
test_that("Invalid options throw errors", {
  expect_error(update(sim, keep_errors = "a"), "`keep_errors` must be of type 'logical'")
  expect_error(update(sim, keep_extra = "a"), "`keep_extra` must be of type 'logical'")
})

sim %<>% set_levels(
  estimator = c("estimator_1"),
  num_patients = c(50, 100),
  ate = c(-7),
  new_level = c(1, 2, 3)
)
test_that("Adding new level variables throws an error", {
  expect_error(update(sim), "Updating a sim cannot include new level variables, only new levels.")
})

# add method, change levels
sim %<>% add_method(estimator_2)
sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 75, 100),
  ate = c(-7)
)

### update adds levels ###
sim %<>% update()
test_that("update() can add new levels", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), prev_nrow + 20)
  expect_equal(sim$results[1,], prev_row1)
})

# back to old levels, add reps
sim %<>% set_levels(
  estimator = c("estimator_1"),
  num_patients = c(50, 100),
  ate = c(-7)
)
sim %<>% set_config(
  num_sim = 10
)

### update adds reps ###
sim %<>% update()
test_that("update() can add reps", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), 2*prev_nrow)
  expect_equal(sim$results[1,], prev_row1)
})

# remove reps and levels
sim %<>% set_levels(
  estimator = c("estimator_1"),
  num_patients = c(50),
  ate = c(-7)
)
sim %<>% set_config(
  num_sim = 4
)

### update removes extra reps/levels ###
sim %<>% update()
test_that("update() can remove extra reps/levels", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), prev_nrow - 6)
  expect_equal(sim$results[1,], prev_row1)
})

# new sim, introduce errors and warnings
sim <- new_sim()

sim %<>% set_script(
  function() {
    if (L$index %% 2 != 0){
      warning('Odd warning.')
      stop('Odd error.')
    }
    x <- sample(c(1,2),1)
    return (list("x"=x))
  }
)

sim %<>% set_levels(
  index = 1:10
)

sim %<>% set_config(
  num_sim = 2,
  parallel = "none"
)

sim %<>% run()
prev_ncol <- c(length(sim$errors), length(sim$warnings))
prev_nrow <- c(nrow(sim$errors), nrow(sim$warnings))
prev_row1 <- list(sim$errors[1,], sim$warnings[1,])

# add levels
sim %<>% set_levels(
  index = 1:20
)

### update properly appends errors and warnings
sim %<>% update()
test_that("update() appends errors and warnings", {
  expect_type(sim$errors, "list")
  expect_type(sim$warnings, "list")
  expect_equal(length(sim$errors), prev_ncol[1])
  expect_equal(length(sim$warnings), prev_ncol[2])
  expect_equal(nrow(sim$errors), 2*prev_nrow[1])
  expect_equal(nrow(sim$warnings), 2*prev_nrow[2])
  expect_equal(sim$errors[1,], prev_row1[[1]])
  expect_equal(sim$warnings[1,], prev_row1[[2]])
})

# back to old levels
sim %<>% set_levels(
  index = 1:10
)

# reduce number of reps

sim %<>% set_config(
  num_sim = 1,
  parallel = "none"
)

### update properly removes extra errors and warnings
sim %<>% update()
test_that("update() removes extra errors and warnings", {
  expect_type(sim$errors, "list")
  expect_type(sim$warnings, "list")
  expect_equal(length(sim$errors), prev_ncol[1])
  expect_equal(length(sim$warnings), prev_ncol[2])
  expect_equal(nrow(sim$errors), 0.5*prev_nrow[1])
  expect_equal(nrow(sim$warnings), 0.5*prev_nrow[2])
  expect_equal(sim$errors[1,], prev_row1[[1]])
  expect_equal(sim$warnings[1,], prev_row1[[2]])
})

# new sim with no levels

sim <- new_sim()

sim %<>% set_script(
  function() {
    x <- sample(c(1,2),1)
    return (list("x"=x))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "none"
)

sim %<>% run()
prev_ncol <- length(sim$results)
prev_nrow <- nrow(sim$results)
prev_row1 <- sim$results[1,]

sim %<>% set_config(
  num_sim = 200,
  parallel = "none"
)

### update doesn't break with no levels
sim %<>% update()
test_that("update() works with no levels", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), 2*prev_nrow)
  expect_equal(sim$results[1,], prev_row1)
})
