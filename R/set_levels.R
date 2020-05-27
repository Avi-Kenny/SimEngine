#' Set simulation levels
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param ... One or more key-value pairs representing simulation levels (see
#'     below)
#' @return The original simulation object with the old set of levels replaced
#'     with the new set
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "rho" = c(0.1, 0.2),
#'   "eta" = c(2,3)
#' )
#' !!!!! continue example
#' @export
set_levels <- function(sim_obj, ...) UseMethod("set_levels")

#' @export
set_levels.simba <- function(sim_obj, ...) {

  # !!!!! Add error handling for "..."

  # Add levels to sim_obj
  if (length(list(...))==0) { stop("No levels supplied") }
  sim_obj$levels <- list(...)

  # Extract names from lists
  levels_shallow <- list()
  levels_types <- c() # Stores whether level is a list (TRUE) or not (FALSE)
  for (i in 1:length(sim_obj$levels)) {
    if (class(sim_obj$levels[[i]])=="list") {
      levels_types <- c(levels_types, TRUE)
      levels_shallow[[names(sim_obj$levels)[i]]] <- names(sim_obj$levels[[i]])
    } else {
      levels_types <- c(levels_types, FALSE)
      levels_shallow[[names(sim_obj$levels)[i]]] <- sim_obj$levels[[i]]
    }
  }
  sim_obj$levels_types <- levels_types # !!!!! Make a container for internal variables (e.g. sim_obj$internals$levels_types)

  # Set up levels_grid and add to sim_obj
  levels_grid_1 <- expand.grid(levels_shallow, stringsAsFactors=FALSE)
  levels_names_1 <- names(levels_grid_1)
  levels_grid_1 <- cbind(1:nrow(levels_grid_1), levels_grid_1)
  names(levels_grid_1) <- c("level_id", levels_names_1)
  levels_grid <- expand.grid(list(
    "level_id" = levels_grid_1$level_id,
    "sim_id" = 1:sim_obj$config$num_sim # !!!!! This will cause problems if set_config is called after set_levels
  ))
  levels_grid <- dplyr::inner_join(levels_grid, levels_grid_1, by="level_id")
  levels_names <- names(levels_grid)
  levels_grid <- dplyr::arrange(levels_grid, level_id, sim_id)
  levels_grid <- cbind(1:nrow(levels_grid), levels_grid)
  names(levels_grid) <- c("sim_uid",levels_names)
  sim_obj$levels_grid <- levels_grid

  return (sim_obj)

}
