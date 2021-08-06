
# Create wrapper functions for testing update_sim_on_cluster()
run_c <- function(ret=FALSE) {

  run_on_cluster(
    first = {
      sim <- new_sim()
      sim %<>% set_config(num_sim=1)
      sim %<>% set_levels(alpha=c(2,3), beta=c(4))
      sim %<>% set_script(function() {
        return (list(sum=(L$alpha+L$beta), prod=(L$alpha*L$beta)))
      })
    },
    main = {sim %<>% run()},
    last = { sim %>% summarize() %>% print() },
    cluster_config = list(js="slurm")
  )

  # The `sim` object should have been created in this environment
  if (ret) { return (sim) }

}

update_c <- function(ret=FALSE) {

  update_sim_on_cluster(
    first = {
      sim <- readRDS('sim.simba')
      sim %<>% set_config(num_sim=2)
      sim %<>% set_levels(alpha = c(2,3), beta = c(4,5))
    },
    main = {sim %<>% update_sim()},
    last = {sim %>% summarize() %>% print()},
    cluster_config = list(js = "slurm")
  )

  # The `sim` object should have been created in this environment
  if (ret) { return (sim) }

}

update_c2 <- function(ret=FALSE) {

  update_sim_on_cluster(
    first = {
      sim <- readRDS('sim.simba')
      sim %<>% set_config(num_sim=1)
      sim %<>% set_levels(alpha = c(2,3,4), beta = c(4,5))
    },
    main = {sim %<>% update_sim()},
    last = {sim %>% summarize() %>% print()},
    cluster_config = list(js = "slurm")
  )

  # The `sim` object should have been created in this environment
  if (ret) { return (sim) }

}

# run everything
Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_c()
Sys.setenv(simba_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
run_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
run_c()
Sys.setenv(simba_run="last")
run_c()

# Run locally
Sys.setenv(simba_run="")
sim <- update_c(TRUE)
test_that("run_on_cluster() works locally", {
  expect_equal(class(sim), "simba")
  expect_equal(sim$results$sum, c(6,7,6,7,7,7,8,8))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
})
rm(sim)

# Incorrect run variable
Sys.setenv(simba_run="asdf123")
test_that("Incorrect 'run' environment variable throws error", {
  expect_error(update_c(), paste("The 'simba_run' environment variable must",
                                 "equal either 'first', 'main', or 'last'."))
})

# Simulate updating on cluster; test 'first' section
Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
update_c()
sim <- readRDS("sim.simba")
test_that("update_sim_on_cluster() 'first' section works", {
  expect_equal(class(sim), "simba")
  #expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(sim$levels$alpha, c(2,3))
  expect_equal(sim$levels$beta, c(4,5))
  expect_equal(dir.exists("simba_results"), TRUE)
})
rm(sim)

# Simulate updating on cluster; test 'main' section
Sys.setenv(simba_run="main")
test_that("Incorrect 'run' environment variable throws error", {
  expect_error(update_c(), "Task ID is missing.")
})
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
update_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
update_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="3")
update_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="4")
update_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="5")
update_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="6")
update_c()

test_that("run_on_cluster() 'main' section works", {
  expect_equal(file.exists("simba_results/r_3.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_4.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_5.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_6.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_7.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_8.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_2.rds"), FALSE)
})
Sys.setenv(SLURM_ARRAY_TASK_ID="")

# Simulate running on cluster; test 'last' section
Sys.setenv(simba_run="last")
update_c()
sim <- readRDS("sim.simba")
output <- readChar("sim_output.txt", file.info("sim_output.txt")$size)

test_that("update_sim_on_cluster() 'last' section works", {
  expect_equal(dir.exists("simba_results"), FALSE)
  expect_equal(sim$results$sum, c(6,7,6,7,7,7,8,8))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(sim$internals$num_sim_cumulative, 8)
  expect_true(sim$internals$update_sim)
  expect_equal(sim$internals$levels_prev$beta, c(4,5))
  expect_equal(grepl("simba output START", output, fixed=TRUE), TRUE)
  expect_equal(grepl("simba output END", output, fixed=TRUE), TRUE)
  expect_equal(grepl("level_id alpha", output, fixed=TRUE), TRUE)
})
Sys.setenv(simba_run="")
rm(sim)
rm(output)






# Simulate a second update updating on cluster; test 'first' section
Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
update_c2()
sim <- readRDS("sim.simba")
test_that("update_sim_on_cluster() 'first' section works", {
  #expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$config$num_sim, 1)
  expect_equal(sim$levels$alpha, c(2,3,4))
})
rm(sim)

# Simulate updating on cluster; test 'main' section
Sys.setenv(simba_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
update_c2()
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
update_c2()

test_that("run_on_cluster() 'main' section works", {
  expect_equal(file.exists("simba_results/r_9.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_10.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_2.rds"), FALSE)
})
Sys.setenv(SLURM_ARRAY_TASK_ID="")

# Simulate running on cluster; test 'last' section
Sys.setenv(simba_run="last")
update_c2()
sim <- readRDS("sim.simba")
output <- readChar("sim_output.txt", file.info("sim_output.txt")$size)

test_that("update_sim_on_cluster() 'last' section works", {
  expect_equal(dir.exists("simba_results"), FALSE)
  expect_equal(sim$results$sum, c(6,7,7,8,8,9))
  expect_equal(sim$results$alpha, c(2,3,2,3,4,4))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 1)
  expect_equal(sim$internals$num_sim_cumulative, 10)
  expect_true(sim$internals$update_sim)
  expect_equal(sim$internals$levels_prev$alpha, c(2,3,4))
  expect_equal(grepl("simba output START", output, fixed=TRUE), TRUE)
  expect_equal(grepl("simba output END", output, fixed=TRUE), TRUE)
  expect_equal(grepl("level_id alpha", output, fixed=TRUE), TRUE)
})
Sys.setenv(simba_run="")
unlink("sim_output.txt")
rm(sim)
rm(output)

# Correct behavior if 'first' fails
# Create wrapper function for testing run_on_cluster()
update_c3 <- function() {
  update_sim_on_cluster(
    first = { stop("Error in 'first'") },
    main = { sim %<>% run("my_script") },
    last = {sim %>% summarize() %>% print() },
    cluster_config = list(sim_var="sim", js="slurm")
  )
}
Sys.setenv(simba_run="first")
test_that("Correct behavior if 'first' fails", {
  expect_error(update_c3(), "Error in 'first'")
  expect_equal(file.exists("sim.simba"), TRUE)
})
# !!!!! what to do if error in first block?

# Sys.setenv(simba_run="main")
# Sys.setenv(SLURM_ARRAY_TASK_ID="1")
# test_that("Correct behavior if 'first' fails", {
#   expect_equal(file.exists("sim.simba"), TRUE)
#   expect_error(update_c3(), paste(
#     "Simulation object was not found. Make sure your 'first' function is not",
#     "producing errors and returns a valid simulation object, and that your",
#     "shell commands are properly sequenced."
#   ))
# })
# Sys.setenv(simba_run="last")
# Sys.setenv(SLURM_ARRAY_TASK_ID="")
# test_that("Correct behavior if 'first' fails", {
#   expect_equal(file.exists("sim.simba"), TRUE)
#   expect_error(update_c3(), paste(
#     "Simulation object was not found. Make sure your 'first' function is not",
#     "producing errors and returns a valid simulation object, and that your",
#     "shell commands are properly sequenced."
#   ))
# })
Sys.sleep(1)
x <- unlink("simba_results", recursive = TRUE)
print(x)
# unlink("sim.simba")


Sys.setenv(simba_run="")
unlink("sim.simba")
