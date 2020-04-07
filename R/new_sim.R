#' Initialize a new simulation object
#'
#' @param config A list. Contains simulation configuration information
#' @return A simulation object of class "simba"
#' @examples
#' sim <- new_sim()
#' @export
new_sim <- function() {

  sim_obj <- list(
    "config" = list(
      "num_sim" = 1,
      "datasets" = "many",
      "parallel" = "inner"
    ),
    "levels" = list(),
    "constants" = list(),
    "creators" = list(),
    "methods" = list()
  )

  class(sim_obj) <- "simba"

  return (sim_obj)

}
