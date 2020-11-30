
# Create wrapper function for testing run_on_cluster()
run_c <- function(ret=FALSE) {

  run_on_cluster(
    first = {
      sim <- new_sim()
      sim %<>% set_config(num_sim=2)
      sim %<>% set_levels(alpha=c(2,3), beta=c(4,5))
      sim %<>% add_script("my_script", function() {
        return(list(sum=(L$alpha+L$beta), prod=(L$alpha*L$beta)))
      })
    },
    main = { sim %<>% run("my_script") },
    last = {sim %>% summary() %>% print() },
    cluster_config = list(sim_var="sim", js="slurm")
  )

  if (ret) { return (sim) }

}

# Incorrect run variable
Sys.setenv(run="asdf123")
test_that("Incorrect 'run' environment variable throws error", {
  expect_error(run_c(), paste("The 'run' environment variable must",
                              "equal either 'first', 'main', or 'last'."))
})

# Run locally
Sys.setenv(run="")
sim <- run_c(TRUE)
test_that("run_on_cluster() works locally", {
  expect_equal(class(sim), "simba")
  expect_equal(sim$results$sum, c(6,6,7,7,7,7,8,8))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
})
rm(sim)

# Simulate running on cluster; test 'first' section
Sys.setenv(run="first")
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
Sys.setenv(run="main")
test_that("Incorrect 'run' environment variable throws error", {
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
Sys.setenv(run="last")
run_c()
sim <- readRDS("sim.simba")
output <- readChar("sim_output.txt", file.info("sim_output.txt")$size)
test_that("run_on_cluster() 'main' section works", {
  expect_equal(dir.exists("simba_results"), FALSE)
  expect_equal(sim$results$sum, c(6,6))
  expect_equal(sim$errors, "No errors")
  expect_equal(sim$config$num_sim, 2)
  expect_equal(grepl("simba output START", output, fixed=TRUE), TRUE)
  expect_equal(grepl("simba output END", output, fixed=TRUE), TRUE)
  expect_equal(grepl("level_id alpha", output, fixed=TRUE), TRUE)
})
Sys.setenv(run="")
unlink("sim.simba")
unlink("sim_output.txt")
rm(sim)
rm(output)
