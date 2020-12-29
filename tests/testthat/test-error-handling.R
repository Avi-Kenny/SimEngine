
# No errors, no warnings

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

msg <- capture_output(
  sim %<>% run("my script")
)

test_that("run() behaves correctly; no errors", {
  expect_equal(substring(msg,nchar(msg)-36,nchar(msg)),
               "Done. No errors or warnings detected.")
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

sim %<>% set_script(
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
  expect_equal(substring(msg, nchar(msg) - 76, nchar(msg)),
               "Done. No errors detected. Warnings detected in 100% of simulation replicates.")
  expect_equal(sim$errors, "No errors")
  expect_type(sim$results, "list")
  expect_type(sim$warnings, "list")
  expect_equal(length(sim$results), 5)
  expect_equal(length(sim$warnings), 5)
  expect_equal(nrow(sim$results), 100)
  expect_equal(nrow(sim$warnings), 100)
  expect_equal(sim$results$sim_uid[1:5], c(1:5))
  expect_equal(sim$results$level_id[1:5], rep(1,5))
  expect_equal(sim$results$sim_id[1:5], c(1:5))
  expect_equal(sim$warnings$sim_uid[1:5], c(1:5))
  expect_equal(sim$warnings$level_id[1:5], rep(1,5))
  expect_equal(sim$warnings$sim_id[1:5], c(1:5))
})



# Some errors

sim <- new_sim()

sim %<>% set_script(
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

pct_error <- nrow(sim$errors)

test_that("run() behaves correctly; some errors", {
  expect_equal(substring(msg, nchar(msg)-103, nchar(msg)),
               paste0("Done. Errors detected in ", pct_error, "% of simulation replicates. Warnings detected in 0% of simulation replicates."))
  expect_type(sim$results, "list")
  expect_type(sim$errors, "list")
  expect_equal(substring(sim$errors[1,"message"], 1, 14), "Lapack routine")
  expect_equal(sim$errors[1,"call"], "solve.default(x)")
})



# All errors

sim <- new_sim()

sim %<>% set_script(
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
  expect_equal(substring(msg, nchar(msg)-104, nchar(msg)),
               "Done. Errors detected in 100% of simulation replicates. Warnings detected in 0% of simulation replicates.")
  expect_type(sim$results, "character")
  expect_type(sim$errors, "list")
  expect_equal(substring(sim$errors[1,"message"], 1, 14), "Lapack routine")
  expect_equal(sim$errors[1,"call"], "solve.default(x)")
})

# stop at error

sim <- new_sim()

sim %<>% set_script(
  function() {
    stop('Stop_at_error test triggered.')
    return (list("x"=1))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "none",
  stop_at_error = TRUE
)

test_that("stop_at_error config option works", {
  expect_error(run(sim), "Stop_at_error test triggered.")
})


# too many cores requested

sim %<>% set_script(
  function() {
    return (list("x"=1))
  }
)

sim %<>% set_config(
  num_sim = 100,
  parallel = "inner",
  parallel_cores = 1000
)

n_available_cores <- parallel::detectCores()
mess <- paste0("1000 cores requested but only ", n_available_cores, " cores available")

test_that("run() throws warning if too many cores requested", {
  expect_warning(run(sim, "my script"), mess)
})
