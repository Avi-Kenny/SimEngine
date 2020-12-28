#' Set simulation levels
#'
#' @description Set one or more simulation levels, which are things that vary
#'     between simulation replicates.
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param ... One or more key-value pairs representing simulation levels (see
#'     example)
#' @param add Only relevant if set_levels() is called multiple times. On the
#'     second call, if add=FALSE (default) the old set of levels will be
#'     replaced by the new set, whereas if add=TRUE the new set of levels will
#'     be merged with the old set. See examples. Also note that you cannot have
#'     a simulation level called 'add', since the name would conflict with this argument
#' @return The original simulation object with the old set of levels replaced
#'     with the new set
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "theta" = c(2, 3)
#' )
#' sim$levels
#' @export
set_levels <- function(sim_obj, ..., add=FALSE) UseMethod("set_levels")

#' @export
set_levels.simba <- function(sim_obj, ..., add=FALSE) {

  handle_errors(sim_obj, "is.simba")
  handle_errors(add, "is.boolean")

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

  # Create levels_grid
  levels_grid <- expand.grid(levels_shallow, stringsAsFactors=FALSE)
  names_1 <- names(levels_grid)
  levels_grid <- cbind(1:nrow(levels_grid), levels_grid)
  names(levels_grid) <- c("level_id", names_1)

  # Attach created variables to sim_obj
  sim_obj$internals$levels_shallow <- levels_shallow
  sim_obj$internals$levels_types <- levels_types
  sim_obj$levels_grid <- levels_grid
  sim_obj$internals$num_sim_total <- nrow(sim_obj$levels_grid) *
                                     sim_obj$config$num_sim

  return (sim_obj)

}
