
# No errors, no warnings

sim <- new_sim()

sim %<>% set_script(
  function() {
    x <- sample(c(1,2),1)
    return (list("x"=x))
  }
)

sim %<>% set_config(num_sim=100, parallel=FALSE)

msg <- capture_messages(
  sim %<>% run()
)

test_that("run() behaves correctly; no errors", {
  expect_equal(msg, "Done. No errors or warnings detected.\n\n")
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$warnings, "No warnings")
  expect_type(sim$results, "list")
  expect_equal(length(sim$results), 6)
  expect_equal(nrow(sim$results), 100)
  expect_equal(sim$results$sim_uid[1:5], c(1:5))
  expect_equal(sim$results$level_id[1:5], rep(1,5))
  expect_equal(sim$results$rep_id[1:5], c(1:5))
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

sim %<>% set_config(num_sim=100, parallel=FALSE)

msg <- capture_messages(
  sim %<>% run()
)

test_that("run() behaves correctly; no errors and some warnings", {
  expect_equal(msg, "Done. No errors detected. Warnings detected in 100% of simulation replicates.\n\n")
  expect_equal(sim$errors, "No errors")
  expect_type(sim$results, "list")
  expect_type(sim$warnings, "list")
  expect_equal(length(sim$results), 6)
  expect_equal(length(sim$warnings), 6)
  expect_equal(nrow(sim$results), 100)
  expect_equal(nrow(sim$warnings), 100)
  expect_equal(sim$results$sim_uid[1:5], c(1:5))
  expect_equal(sim$results$level_id[1:5], rep(1,5))
  expect_equal(sim$results$rep_id[1:5], c(1:5))
  expect_equal(sim$warnings$sim_uid[1:5], c(1:5))
  expect_equal(sim$warnings$level_id[1:5], rep(1,5))
  expect_equal(sim$warnings$rep_id[1:5], c(1:5))
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

sim %<>% set_config(num_sim=100, parallel=FALSE)

msg <- capture_messages(
  sim %<>% run()
)

pct_error <- nrow(sim$errors)

test_that("run() behaves correctly; some errors", {
  expect_equal(msg, paste0("Done. Errors detected in ", pct_error, "% of simulation replicates. Warnings detected in 0% of simulation replicates.\n\n"))
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

sim %<>% set_config(num_sim=100, parallel=FALSE)

msg <- capture_messages(
  sim %<>% run()
)

test_that("run() behaves correctly; all errors", {
  expect_equal(msg, "Done. Errors detected in 100% of simulation replicates. Warnings detected in 0% of simulation replicates.\n\n")
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

sim %<>% set_config(num_sim=100, parallel=FALSE, stop_at_error=TRUE)

# Can't run a sim twice
sim <- new_sim()
sim %<>% set_script(function() { return(list(x=1)) })
sim %<>% run()
test_that("Can't run a simulation twice", {
  expect_error(run(sim), paste0("This simulation has already been run; use upd",
                                "ate_sim\\(\\) to add or remove replicates"))
})

# !!!!! For some reason, this test works when run manually but doesn't work
#       when run with SHFT+CTRL+T
# test_that("stop_at_error config option works", {
#   expect_error(run(sim), "Stop_at_error test triggered.")
# })
