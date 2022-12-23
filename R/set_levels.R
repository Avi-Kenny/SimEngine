#' Set simulation levels
#'
#' @description Set one or more simulation levels, which are things that vary
#'     between simulation replicates.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param ... One or more key-value pairs representing simulation levels. Each
#'     value can either be a vector (for simple levels) or a list of lists (for
#'     more complex levels). See examples.
#' @param .add Only relevant if \code{\link{set_levels}} is called twice or
#'     more. On the second call, if add=FALSE (default) the old set of levels
#'     will be replaced by the new set, whereas if add=TRUE the new set of
#'     levels will be merged with the old set. See examples.
#' @param .keep An integer vector specifying which level combinations to keep;
#'     see examples.
#' @return The original simulation object with the old set of levels replaced
#'     with the new set
#' @examples
#' # Basic usage is as follows:
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "theta" = c(2, 3)
#' )
#' sim$levels
#'
#' # More complex levels can be set using lists:
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "theta" = c(2, 3),
#'   "method" = list(
#'     "spline1" = list(knots=c(2,4), slopes=c(0.1,0.4)),
#'     "spline2" = list(knots=c(1,5), slopes=c(0.2,0.3))
#'   )
#' )
#' sim$levels
#'
#' # By default, set_levels will overwrite old levels if it is called twice:
#' sim %<>% set_levels(alpha=c(1,2), beta=c(5,6))
#' sim %<>% set_levels(alpha=c(3,4), gamma=c(7,8))
#' sim$levels
#'
#' # To merge the old levels with the new levels instead, specify .add=TRUE:
#' sim %<>% set_levels(alpha=c(1,2), beta=c(5,6))
#' sim %<>% set_levels(alpha=c(3,4), gamma=c(7,8), .add=TRUE)
#' sim$levels
#'
#' # If you don't want to run simulations for all level combinations, use the
#' # .keep option. First, set the levels normally. Second, view the
#' # sim$levels_grid dataframe to examine the level combinations and the
#' # associated level_id values. Third, call set_levels again with the .keep
#' # option to specify which levels to keep (via a vector of level_id values).
#' sim %<>% set_levels(alpha=c(1,2,3), beta=c(5,6))
#' sim$levels_grid
#' sim %<>% set_levels(.keep=c(1,2,6))
#' sim$levels_grid
#' @export
set_levels <- function(sim, ..., .add=F, .keep=NA) {
  UseMethod("set_levels")
}

#' @export
set_levels.sim_obj <- function(sim, ..., .add=F, .keep=NA) {

  handle_errors(sim, "is.sim_obj")
  handle_errors(.add, "is.boolean")
  if (!is.na(.keep[[1]])) {
    handle_errors(.keep, "is.numeric.vec")
    if (.add) { stop("set_levels cannot be called with both .add=T and .keep") }
    if (any(!(.keep %in% sim$levels_grid$level_id))) {
      stop(".keep argument incorrectly specified")
    }
  }
  if (length(list(...))==0 && is.na(.keep[[1]])) { stop("No levels supplied") }

  # Merge with existing levels if .add=TRUE; otherwise, overwrite
  if (.add) {

    new_list <- list(...)
    for (i in 1:length(new_list)) {
      sim$levels[[names(new_list[i])]] <- c(
        sim$levels[[names(new_list[i])]],
        new_list[[i]]
      )
    }

  } else {

    if (length(list(...))>0) { sim$levels <- list(...) }

  }

  if (length(list(...))>0) {

    if (!(is.list(sim$levels)) ||
        !(length(names(sim$levels[which(names(sim$levels) != "")])) ==
          length(sim$levels))) {
      stop("Simulation levels must be key-value pairs.")
    }

    # handle_errors(sim$levels, "is.named.list") # !!!!! This is not implemented

    # Extract names from lists
    levels_shallow <- list()
    levels_types <- c() # Stores whether level is a list (TRUE) or not (FALSE)
    for (i in 1:length(sim$levels)) {
      if (methods::is(sim$levels[[i]],"list")) {
        # if the level is a list, it must be a named list of lists
        # first, make sure it has names
        if (length(names(sim$levels[[i]])) != length(sim$levels[[i]]) ||
            "" %in% names(sim$levels[[i]])) {
          stop("Each item in a list level must have a name.")
        }
        # then, make sure each item in the list is, itself, a list
        for (j in 1:length(sim$levels[[i]])) {
          if (!is.list(sim$levels[[i]][[j]])) {
            stop("Each item in a list level must be a list.")
          }
        }
        levels_types <- c(levels_types, TRUE)
        levels_shallow[[names(sim$levels)[i]]] <- names(sim$levels[[i]])
      } else {
        levels_types <- c(levels_types, FALSE)
        levels_shallow[[names(sim$levels)[i]]] <- sim$levels[[i]]
      }
    }

    # Create levels_grid
    levels_grid <- expand.grid(levels_shallow, stringsAsFactors=FALSE)
    names_1 <- names(levels_grid)
    levels_grid <- cbind(1:nrow(levels_grid), levels_grid)
    names(levels_grid) <- c("level_id", names_1)

    # Attach created variables to sim
    sim$internals$levels_shallow <- levels_shallow
    sim$internals$levels_types <- levels_types
    sim$levels_grid <- levels_grid

  }

  if (!is.na(.keep[[1]])) {
    rows_to_keep <- which(sim$levels_grid$level_id %in% .keep)
    sim$levels_grid <- sim$levels_grid[rows_to_keep,]
  }

  sim$vars$num_sim_total <- nrow(sim$levels_grid) * sim$config$num_sim

  return (sim)

}
