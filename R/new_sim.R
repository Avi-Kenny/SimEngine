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
      "num_sim" = 1000,
      "datasets" = "many",
      "parallel" = "none",
      "packages" = c(),
      "progress" = "none",
      "stop_at_error" = FALSE
    ),
    "constants" = list(),
    "levels" = list("no levels"=TRUE),
    "levels_grid" = data.frame("level_id"=1),
    "internals" = list(
      "levels_types" = FALSE,
      "levels_shallow" = list("no levels"=TRUE)
    ),
    "creators" = list(),
    "methods" = list(),
    "scripts" = list(),
    "results" = NULL,
    "errors" = NULL
  )

  class(sim_obj) <- "simba"

  return (sim_obj)

}
