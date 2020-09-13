
# Blank summary

sim <- new_sim()

test_that("Summary of blank sim object throws an error", {
  expect_error(summary(sim), "Simulation has not been run yet.")
})



# Summary of object with 100% errors

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
  num_sim = 10
)

sim %<>% run("my script")

test_that("Summary of blank sim object throws an error", {
  expect_error(summary(sim), "100% of simulations had errors.")
})
