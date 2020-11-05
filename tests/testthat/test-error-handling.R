
# No errors, no warnings

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    x <- sample(c(1,2),1)
    return (list("x"=x))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "none"
)

msg <- capture_output(
  sim %<>% run("my script")
)

test_that("run() behaves correctly; no errors", {
  expect_equal(msg, "Done. No errors or warnings detected.")
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$warnings, "No warnings")
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), 5)
  expect_equal(nrow(sim$results), 100)
  expect_equal(sim$results$sim_uid[1:5], c(1:5))
  expect_equal(sim$results$level_id[1:5], rep(1,5))
  expect_equal(sim$results$sim_id[1:5], c(1:5))
})


# No errors, some warnings (multiple per sim)

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    warning('One warning.')
    warning('Two warnings.')
    x <- sample(c(1,2),1)
    return (list("x"=x))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "none"
)

msg <- capture_output(
  sim %<>% run("my script")
)

test_that("run() behaves correctly; no errors and some warnings", {
  expect_equal(substring(msg, 1, 43),
               "Done. No errors detected. Warnings detected")
  expect_equal(sim$errors, "No errors")
  expect_type(sim$results, "list")
  expect_type(sim$warnings, "list")
  expect_equal(length(sim$results), 5)
  expect_equal(length(sim$warnings), 5)
  expect_equal(nrow(sim$results), 100)
  expect_equal(nrow(sim$warnings), 200)
  expect_equal(sim$results$sim_uid[1:5], c(1:5))
  expect_equal(sim$results$level_id[1:5], rep(1,5))
  expect_equal(sim$results$sim_id[1:5], c(1:5))
  expect_equal(sim$warnings$sim_uid[1:5], c(1,1,2,2,3))
  expect_equal(sim$warnings$level_id[1:5], rep(1,5))
  expect_equal(sim$warnings$sim_id[1:5], c(1,1,2,2,3))
})



# Some errors

sim <- new_sim()

sim %<>% add_script(
  "my script",
  function() {
    x <- matrix(
      c(sample(c(1,2),1), sample(c(1,2),1), sample(c(1,2),1), sample(c(1,2),1)),
      nrow = 2
    )
    x <- solve(x)
    return (list("x"=x[1,1]))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "none"
)

msg <- capture_output(
  sim %<>% run("my script")
)

test_that("run() behaves correctly; some errors", {
  expect_equal(substring(msg, 1, 21), "Done. Errors detected")
  expect_type(sim$results, "list")
  expect_type(sim$errors, "list")
  expect_equal(substring(sim$errors[1,"message"], 1, 14), "Lapack routine")
  expect_equal(sim$errors[1,"call"], "solve.default(x)")
})



# All errors

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
  num_sim = 100,
  parallel = "none"
)

msg <- capture_output(
  sim %<>% run("my script")
)

test_that("run() behaves correctly; all errors", {
  expect_equal(substring(msg, 1, 21), "Done. Errors detected")
  expect_type(sim$results, "character")
  expect_type(sim$errors, "list")
  expect_equal(substring(sim$errors[1,"message"], 1, 14), "Lapack routine")
  expect_equal(sim$errors[1,"call"], "solve.default(x)")
})
