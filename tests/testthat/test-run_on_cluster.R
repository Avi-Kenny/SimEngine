
# Create wrapper function for testing run_on_cluster()
run_c <- function(ret=FALSE, cmplx=FALSE) {

  if (cmplx==FALSE) {
    run_on_cluster(
      first = {
        sim <- new_sim()
        sim %<>% set_config(num_sim=2)
        sim %<>% set_levels(alpha=c(2,3), beta=c(4,5))
        sim %<>% set_script(function() {
          list(
            sum = L$alpha+L$beta,
            prod = L$alpha*L$beta
          )
        })
      },
      main = { sim %<>% run() },
      last = { sim %>% summarize() %>% print() },
      cluster_config = list(js="slurm")
    )
  } else {
    run_on_cluster(
      first = {
        sim <- new_sim()
        sim %<>% set_config(num_sim=2)
        sim %<>% set_levels(alpha=c(2,3), beta=c(4,5))
        sim %<>% set_script(function() {
          list(
            sum = L$alpha+L$beta,
            prod = L$alpha*L$beta,
            .complex = list(vec = c(L$alpha, L$beta))
          )
        })
      },
      main = { sim %<>% run() },
      last = { sim %>% summarize() %>% print() },
      cluster_config = list(js="slurm")
    )
  }


  # The `sim` object should have been created in this environment
  if (ret) { return (sim) }

}

# Incorrect run variable
Sys.setenv(simba_run="asdf123")
test_that("Incorrect 'run' environment variable throws error", {
  expect_error(run_c(), paste("The 'simba_run' environment variable must",
                              "equal either 'first', 'main', or 'last'."))
})

# Run locally
Sys.setenv(simba_run="")
sim <- run_c(TRUE)
test_that("run_on_cluster() works locally", {
  expect_equal(class(sim), "simba")
  expect_equal(sim$results$sum, c(6,6,7,7,7,7,8,8))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
})
rm(sim)
unlink("sim.simba")

# Simulate running on cluster; test 'first' section
Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_c()
sim <- readRDS("sim.simba")
test_that("run_on_cluster() 'first' section works", {
  expect_equal(class(sim), "simba")
  expect_equal(sim$results, "Simulation has not been run yet.")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(dir.exists("simba_results"), TRUE)
})
rm(sim)

# Simulate running on cluster; test 'main' section
Sys.setenv(simba_run="main")
test_that("Missing task ID throws error", {
  expect_error(run_c(), "Task ID is missing.")
})
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
run_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
run_c()
test_that("run_on_cluster() 'main' section works", {
  expect_equal(file.exists("simba_results/r_1.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_2.rds"), TRUE)
  expect_equal(file.exists("simba_results/r_3.rds"), FALSE)
})
Sys.setenv(SLURM_ARRAY_TASK_ID="")

# Simulate running on cluster; test 'last' section
Sys.setenv(simba_run="last")
run_c()
sim <- readRDS("sim.simba")
output <- readChar("sim_output.txt", file.info("sim_output.txt")$size)

test_that("run_on_cluster() 'last' section works", {
  expect_equal(dir.exists("simba_results"), FALSE)
  expect_equal(sim$results$sum, c(6,6))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(grepl("simba output START", output, fixed=TRUE), TRUE)
  expect_equal(grepl("simba output END", output, fixed=TRUE), TRUE)
  expect_equal(grepl("level_id alpha", output, fixed=TRUE), TRUE)
})
Sys.setenv(simba_run="")
unlink("sim.simba")
unlink("sim_output.txt")
unlink("sim_results", recursive = TRUE)
rm(sim)
rm(output)

# Correct behavior if 'first' fails
# Create wrapper function for testing run_on_cluster()
run_c2 <- function() {
  run_on_cluster(
    first = { stop("Error in 'first'") },
    main = { sim %<>% run("my_script") },
    last = {sim %>% summarize() %>% print() },
    cluster_config = list(sim_var="sim", js="slurm")
  )
}
Sys.setenv(simba_run="first")
test_that("Correct behavior if 'first' fails", {
  expect_error(run_c2(), "Error in 'first'")
  expect_equal(file.exists("sim.simba"), FALSE)
})
Sys.setenv(simba_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
test_that("Correct behavior if 'first' fails", {
  expect_equal(file.exists("sim.simba"), FALSE)
  expect_error(run_c2(), paste(
    "Simulation object was not found. Make sure your 'first' function is not",
    "producing errors and returns a valid simulation object, and that your",
    "shell commands are correct and properly sequenced."
  ))
})
Sys.setenv(simba_run="last")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
test_that("Correct behavior if 'first' fails", {
  expect_equal(file.exists("sim.simba"), FALSE)
  expect_error(run_c2(), paste(
    "Simulation object was not found. Make sure your 'first' function is not",
    "producing errors and returns a valid simulation object, and that your",
    "shell commands are correct and properly sequenced."
  ))
})
unlink("simba_results")
Sys.setenv(simba_run="")

# Simulate running locally and on cluster with complex return data
Sys.setenv(simba_run="")
sim <- run_c(ret=TRUE, cmplx=TRUE)
test_that("run_on_cluster() works locally", {
  expect_equal(class(sim), "simba")
  expect_equal(sim$results$sum, c(6,6,7,7,7,7,8,8))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(length(sim$results_complex), 8)
  expect_equal(class(sim$results_complex), "list")
  expect_equal(sim$results_complex[[1]]$vec, c(2,4))
})
rm(sim)

Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_c(cmplx=TRUE)
Sys.setenv(simba_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
run_c(cmplx=TRUE)
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
run_c(cmplx=TRUE)
Sys.setenv(SLURM_ARRAY_TASK_ID="")
Sys.setenv(simba_run="last")
run_c(cmplx=TRUE)
sim <- readRDS("sim.simba")
output <- readChar("sim_output.txt", file.info("sim_output.txt")$size)
test_that("run_on_cluster() works with complex data", {
  expect_equal(dir.exists("simba_results"), FALSE)
  expect_equal(sim$results$sum, c(6,6))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(grepl("simba output START", output, fixed=TRUE), TRUE)
  expect_equal(grepl("simba output END", output, fixed=TRUE), TRUE)
  expect_equal(grepl("level_id alpha", output, fixed=TRUE), TRUE)
  expect_equal(length(sim$results_complex), 2)
  expect_equal(class(sim$results_complex), "list")
  expect_equal(sim$results_complex[[1]]$vec, c(2,4))
})
Sys.setenv(simba_run="")
unlink("sim.simba")
unlink("sim_output.txt")
unlink("sim_results", recursive = TRUE)
rm(sim)
rm(output)
# !!!!! test update_on_cluster with complex data




# Create wrapper function for testing run_on_cluster() with warnings
run_c <- function(ret=FALSE) {

  run_on_cluster(
    first = {
      sim <- new_sim()
      sim %<>% set_config(num_sim=2)
      sim %<>% set_levels(alpha=c(2,3), beta=c(4,5))
      sim %<>% set_script(function() {
        warning("This is a test warning.")
        list(
          sum = L$alpha+L$beta,
          prod = L$alpha*L$beta
        )
      })
    },
    main = { sim %<>% run() },
    last = { sim %>% summarize() %>% print() },
    cluster_config = list(js="slurm")
  )

  # The `sim` object should have been created in this environment
  if (ret) { return (sim) }

}
# Simulate running on cluster; produce warnings
Sys.setenv(simba_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_c()
Sys.setenv(simba_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
run_c()
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
run_c()
test_that("run_on_cluster() produces warning files", {
  expect_equal(file.exists("simba_results/w_1.rds"), TRUE)
  expect_equal(file.exists("simba_results/w_2.rds"), TRUE)
  expect_equal(file.exists("simba_results/w_3.rds"), FALSE)
})
Sys.setenv(SLURM_ARRAY_TASK_ID="")
Sys.setenv(simba_run="last")
run_c()
sim <- readRDS("sim.simba")

test_that("run_on_cluster() 'last' section works", {
  expect_equal(sim$results$sum, c(6,6))
  expect_equal(sim$warnings$message, rep("This is a test warning.", 2))
  expect_equal(sim$errors, "No errors")
})
Sys.setenv(simba_run="")
unlink("sim.simba")
unlink("sim_output.txt")
unlink("sim_results", recursive = TRUE)
rm(sim)
