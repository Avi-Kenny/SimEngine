
sim <- new_sim()
ls(sim$internals$env)

sim %<>% add_method("one_norm", function(){ rnorm(1) })
sim %<>% add_method("access_level", function(){ L$alpha })
sim %<>% add_method("access_constant", function(){ C$beta })

sim %<>% add_creator("create_data", function(){
  x <- one_norm()
  y <- access_level()
  z <- access_constant()
  return (list(x=x,y=y,z=z))
})

sim %<>% set_levels(alpha = c(1,2))
sim %<>% add_constants(beta = 3)
sim %<>% set_config(num_sim=1)
# sim %<>% set_config(num_sim=1, parallel="outer")

sim %<>% set_script(function() {
  d <- create_data()
  return (d)
})
ls(sim$internals$env)

# sim$internals$env
# environment(sim$script)
# environment(sim$methods$one_norm)

ls(sim$internals$env)

sim %<>% run()

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })


# !!!!! DWD !!!!!
(function() {

  env <- new.env()

  x <- function(t) {2*t}
  print(environment(x))
  environment(x) <- env
  print(environment(x))
  print(env)

})()

# !!!!! Make sure this works with parallel="outer"; add tests


e1 <- new.env()
e2 <- new.env()
e1$func <- function(){3}
e2$func <- function(){4}
do.call(what="func", args=list(), envir=e1)
do.call(what="func", args=list(), envir=e2)



