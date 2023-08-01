#' Set simulation levels
#'
#' @description Set one or more simulation levels, which are things that vary
#'     between simulation replicates.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param ... One or more key-value pairs representing simulation levels. Each
#'     value can either be a vector (for simple levels) or a list of lists (for
#'     more complex levels). See examples.
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
#' sim <- new_sim()
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
#' # If you don't want to run simulations for all level combinations, use the
#' # .keep option. First, set the levels normally. Second, view the
#' # sim$levels_grid dataframe to examine the level combinations and the
#' # associated level_id values. Third, call set_levels again with the .keep
#' # option to specify which levels to keep (via a vector of level_id values).
#' sim <- new_sim()
#' sim %<>% set_levels(alpha=c(1,2,3), beta=c(5,6))
#' sim$levels_grid
#' #>   level_id alpha beta
#' #> 1        1     1    5
#' #> 2        2     2    5
#' #> 3        3     3    5
#' #> 4        4     1    6
#' #> 5        5     2    6
#' #> 6        6     3    6
#' sim %<>% set_levels(.keep=c(1,2,6))
#' sim$levels_grid
#' @export
set_levels <- function(sim, ..., .keep=NA) {
  UseMethod("set_levels")
}

#' @export
set_levels.sim_obj <- function(sim, ..., .keep=NA) {

  if (!is.na(.keep[[1]])) { handle_errors(.keep, "is.numeric.vec") }
  if (length(list(...))==0 && is.na(.keep[1])) { stop("No levels supplied") }

  if (length(list(...))>0) {

    sim$levels <- list(...)

    # This flag denotes that set_levels has not previously been run
    first_run <- as.logical(sim$internals$level_names[1]=="no_levels")

    if (!first_run) {

      # Disallow changing level variables
      if (!identical(sort(sim$internals$level_names),sort(names(sim$levels)))) {
        stop(paste0("You cannot change the level variables after they are init",
                    "ially set"))
      }

    }

    # Set level_names vector (this defines the order of the levels)
    sim$internals$level_names <- names(sim$levels)

    if (!(is.list(sim$levels)) ||
        !(length(names(sim$levels[which(names(sim$levels) != "")])) ==
          length(sim$levels))) {
      stop("Simulation levels must be key-value pairs.")
    }

    # Create additional levels objects
    levels_shallow <- list()
    levels_types <- c()
    for (i in 1:length(sim$levels)) {

      if (methods::is(sim$levels[[i]],"list")) {

        # If the level is a list, it must be a named list of lists
        # first, make sure it has names
        if (length(names(sim$levels[[i]])) != length(sim$levels[[i]]) ||
            "" %in% names(sim$levels[[i]])) {
          stop("Each item in a list level must have a name.")
        }

        # Make sure each item in the list is itself a list
        for (j in 1:length(sim$levels[[i]])) {
          if (!is.list(sim$levels[[i]][[j]])) {
            stop("Each item in a list level must be a list.")
          }
        }

        levels_types <- c(levels_types, T)
        levels_shallow[[names(sim$levels)[i]]] <- names(sim$levels[[i]])

      } else {

        levels_types <- c(levels_types, F)
        levels_shallow[[names(sim$levels)[i]]] <- sim$levels[[i]]

      }

    }

    # Attach levels_shallow and levels_types to sim
    sim$internals$levels_shallow <- levels_shallow
    sim$internals$levels_types <- levels_types

    # Create levels_grid
    levels_grid <- expand.grid(levels_shallow, stringsAsFactors=F)
    if (first_run) {

      level_ids <- c(1:nrow(levels_grid))

    } else {

      # Pull in old level_ids from levels_grid_historical
      level_ids <- dplyr::left_join(
        levels_grid,
        sim$internals$levels_grid_historical,
        by = sim$internals$level_names
      )$level_id

      # Variable that stores greatest historical level_id
      max_hist_level_id <- max(sim$internals$levels_grid_historical$level_id)

      # Create new level_ids if needed
      if(any(is.na(level_ids))) {
        num_na <- sum(is.na(level_ids))
        level_ids[which(is.na(level_ids))] <- max_hist_level_id + c(1:num_na)
      }

    }
    levels_grid <- cbind(level_ids, levels_grid)
    names(levels_grid) <- c("level_id", sim$internals$level_names)
    sim$levels_grid <- levels_grid

    # Update levels_grid_historical
    if (first_run) {
      sim$internals$levels_grid_historical <- sim$levels_grid
    } else {
      for (i in c(1:nrow(sim$levels_grid))) {
        if (sim$levels_grid$level_id[i]>max_hist_level_id) {
          sim$internals$levels_grid_historical[
            round(nrow(sim$internals$levels_grid_historical)+1),
          ] <- sim$levels_grid[i,]
        }
      }
    }

  }

  # Discard levels that are not needed
  if (!is.na(.keep[[1]])) {
    if (any(!(.keep %in% sim$levels_grid$level_id))) {
      stop(".keep argument incorrectly specified")
    }
    rows_to_keep <- which(sim$levels_grid$level_id %in% .keep)
    sim$levels_grid <- sim$levels_grid[rows_to_keep,]
  }

  # Update batch_ids, sim_uid_grid, num_sim_total
  sim$internals$level_batch_map <- update_level_batch_map(sim)
  sim$internals$sim_uid_grid <- update_sim_uid_grid(sim)
  sim$vars$num_sim_total <- num_sim_total(sim)

  return (sim)

}
