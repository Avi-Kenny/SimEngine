
# Create wrapper function for testing run_on_cluster()
run_c <- function() {

  run_on_cluster(

    first = {
      sim <- new_sim()
      sim %<>% set_config(num_sim=5)
      sim %<>% set_levels(mu=c(10,20))
      sim %<>% add_creator("my_creator", function(x) {rnorm(5, mean=x)})
      sim %<>% add_script("my_script", function() {
        return(list(x=rnorm(1,L$mu), y=rnorm(1)))
      })
    },

    main = {
      sim %<>% run("my_script")
    },

    last = {
      sim %>% summary() %>% print()
    },

    cluster_config = list(sim_var="sim", js="slurm")

  )

}

# Simulate environment variables
Sys.setenv(run="asdf123")
test_that("Incorrect 'run' environment variable throws error", {
  expect_error(run_c(), paste("The 'run' environment variable must equal",
                              "either 'first', 'main', or 'last'."))
})




Sys.setenv(run="first")             # First
Sys.setenv(run="main")              # Main
Sys.setenv(SLURM_ARRAY_TASK_ID="1") # Main
Sys.setenv(run="last")              # Last



test_that("stop_at_error config option works", {
  expect_error(run(sim, "my script"), "Stop_at_error test triggered.")
})



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
