
sim <- new_sim()

sim %<>% add_method("return_one", function(){ 1 })
sim %<>% add_method("access_level", function(){ L$alpha })
sim %<>% add_method("access_constant", function(){ C$beta })
sim %<>% add_creator("create_data", function(){
  x <- return_one()
  y <- access_level()
  z <- access_constant()
  return (list(x=x,y=y,z=z))
})

sim %<>% set_levels(alpha=c(2,3))
sim %<>% add_constants(beta=4)
sim %<>% set_config(num_sim=1)

sim %<>% set_script(function() {
  d <- create_data()
  return (d)
})

sim2 <- sim
sim2 %<>% set_config(parallel="outer", n_cores=2)

sim %<>% run()
sim2 %<>% run()

test_that("Simulation ran and all objects were accessible (serial)", {
  expect_equal(sim$results[1,"x"], 1)
  expect_equal(sim$results[1,"y"], 2)
  expect_equal(sim$results[2,"y"], 3)
  expect_equal(sim$results[1,"z"], 4)
})

test_that("Simulation ran and all objects were accessible (parallel)", {
  expect_equal(sim2$results[1,"x"], 1)
  expect_equal(sim2$results[1,"y"], 2)
  expect_equal(sim$results[2,"y"], 3)
  expect_equal(sim$results[1,"z"], 4)
})
