#' Initialize a new simulation object
#'
#' @param config A list. Contains simulation configuration information
#' @return A simulation object of class "simba"
#' @examples
#' sim <- new_sim()
#' @export
new_sim <- function(config=NA) {

  sim_obj <- list(

    "config" = list(
      "num_sim" = 5,
      "datasets" = "many",
      "parallel" = "inner"
    ),

    "levels" = list(
      "dimension_1" = c(33,44,55),
      "dimension_2" = c(10,100,1000)
    ),

    "constants" = list(
      "n" = 10,
      "constant_2" = 123
    )

  )

  class(sim_obj) <- "simba"

  return (sim_obj)

}
