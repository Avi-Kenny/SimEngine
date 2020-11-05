
# new_sim()
sim <- new_sim()

test_that("new_sim() creates correctly-specified object", {
  expect_equal(sim$config$num_sim, 1000)
  expect_equal(sim$config$parallel, "inner")
  expect_equal(sim$levels, list("no levels"=TRUE))
  expect_equal(sim$results, "Simulation has not been run yet")
  expect_equal(sim$errors, "Simulation has not been run yet")
  expect_equal(sim$warnings, "Simulation has not been run yet")
})



# add_creator()

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

sim %<>% add_creator(create_rct_data)
df <- sim$creators[[1]](5)

test_that("add_creator() works", {
  expect_type(sim$creators, "list")
  expect_equal(length(sim$creators), 1)
  expect_type(sim$creators[[1]], "closure")
  expect_equal(df$patient_id, c(1:5))
})




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
sim %<>% add_method(estimator_2)

sim %<>% set_levels(
  estimator = c("estimator_1", "estimator_2"),
  num_patients = c(50, 100, 200)
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
  num_sim = 100,
  parallel = "none"
)

sim %<>% run("my script")

sim %>% summary()

sim %>% summary(
  bias = list(name="bias_ate", truth=-7, estimate="estimate"),
  mse = list(name="mse_ate", truth=-7, estimate="estimate")
)
