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

### update_sim handles errors ###
test_that("Invalid options throw errors", {
  expect_error(update_sim(sim, keep_errors = "a"), "`keep_errors` must be of type 'logical'")
  expect_error(update_sim(sim, keep_extra = "a"), "`keep_extra` must be of type 'logical'")
})

sim %<>% set_levels(
  estimator = c("estimator_1"),
  num_patients = c(50, 100),
  ate = c(-7),
  new_level = c(1, 2, 3)
)
test_that("Adding new level variables throws an error", {
  expect_error(update_sim(sim), "Updating a sim cannot include new level variables, only new levels.")
})

# change levels
sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 75, 100),
  ate = c(-7)
)

### update_sim adds levels ###
sim %<>% update_sim()
test_that("update_sim() can add new levels", {
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

### update_sim adds reps ###
sim %<>% update_sim()
test_that("update_sim() can add reps", {
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

### update_sim removes extra reps/levels ###
suppressWarnings({
  sim %<>% update_sim()
})

test_that("update_sim() can remove extra reps/levels", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), prev_nrow - 6)
  expect_equal(sim$results[1,], prev_row1)
})

# new sim, introduce errors and warnings
sim <- new_sim()

sim %<>% set_script(
  function() {
    if (L$index %% 2 != 0) {
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

### update_sim properly appends errors and warnings
sim %<>% update_sim()
test_that("update_sim() appends errors and warnings", {
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

### update_sim properly removes extra errors and warnings
suppressWarnings({
  sim %<>% update_sim()
})
test_that("update_sim() removes extra errors and warnings", {
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

### update_sim doesn't break with no levels
sim %<>% update_sim()
test_that("update_sim() works with no levels", {
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), prev_ncol)
  expect_equal(nrow(sim$results), 2*prev_nrow)
  expect_equal(sim$results[1,], prev_row1)
})
