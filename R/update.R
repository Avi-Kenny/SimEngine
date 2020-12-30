#' Update a simulation
#'
#' @description !!!!! TO DO
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}, that has already been run by the \link{run} function
#' @param keep_errors logical (TRUE by default); if TRUE, do not try to re-run
#'     simulation reps that results in errors previously; if FALSE, attempt to
#'     run those reps again
#' @param keep_extra logical (FALSE by default); if TRUE, keep previously run
#'     simulation reps even if they exceed the current num_sim in config or are from
#'     a level that has been dropped; if FALSE, drop excess reps (starting from the last rep
#'     for that particular simulation level)
#' @examples
#' !!!!! TO DO
#' @export
update <- function(sim_obj,
                   keep_errors = TRUE,
                   keep_extra = FALSE) UseMethod("update")

#' @export
update.simba <- function(sim_obj,
                         keep_errors = TRUE,
                         keep_extra = FALSE) {

  # error handle invalid options
  handle_errors(keep_errors, "is.boolean")
  handle_errors(keep_extra, "is.boolean")

  # make sorted list of current levels and previous levels
  sorted_prev_levels <- sim_obj$internals$levels_prev[order(names(sim_obj$internals$levels_prev))]
  sorted_curr_levels <- sim_obj$internals$levels_shallow[order(names(sim_obj$internals$levels_shallow))]

  # disallow adding new level variables
  if (sum(names(sorted_prev_levels) != names(sorted_curr_levels)) != 0){
    stop("Updating a sim cannot include new level variables, only new levels.")
  }

  # make grid of previously run levels / sim_ids
  prev_levels_grid_big <- sim_obj$internals$levels_grid_big
  prev_levels_grid <- prev_levels_grid_big[,-c(1,3), drop = FALSE] %>% dplyr::distinct()
  if (!("no levels" %in% names(sorted_prev_levels))){
    sim_obj$levels_grid <- dplyr::left_join(sim_obj$levels_grid[,-1, drop=FALSE],
                                            prev_levels_grid,
                                            by = names(sorted_prev_levels))
    max_levelid <- max(prev_levels_grid$level_id)
    #if (nrow(sim_obj$levels_grid) > max_levelid){
    if (sum(is.na(sim_obj$levels_grid$level_id)) > 0){
      new_levelids <- (max_levelid + 1):(max_levelid + sum(is.na(sim_obj$levels_grid$level_id)))
      sim_obj$levels_grid$level_id[is.na(sim_obj$levels_grid$level_id)] <- new_levelids
    }
  }

  # Create levels_grid_big
  levels_grid_big <- create_levels_grid_big(sim_obj)
  levels_grid_big <- dplyr::left_join(levels_grid_big[,-1, drop=FALSE],
                                      prev_levels_grid_big,
                                      by = names(levels_grid_big)[-1])
  max_uid <- max(prev_levels_grid_big$sim_uid)
  if (sum(is.na(levels_grid_big$sim_uid)) > 0){
    new_uids <- (max_uid + 1):(max_uid + sum(is.na(levels_grid_big$sim_uid)))
    levels_grid_big$sim_uid[is.na(levels_grid_big$sim_uid)] <- new_uids
  }

  col_order <- c("sim_uid", "level_id", "sim_id", names(sim_obj$internals$levels_shallow))
  levels_grid_big <- levels_grid_big[,col_order]

  # if re-running error reps, limit the prev_levels_grid to only those in results and revert the error df
  if (!keep_errors){
    prev_levels_grid_big <- dplyr::semi_join(prev_levels_grid_big,
                                         sim_obj$results,
                                         by = names(prev_levels_grid_big))
    sim_obj$errors <- "No errors"
  }

  # get levels / sim_ids that have not previously been run
  not_run <- dplyr::anti_join(levels_grid_big,
                              prev_levels_grid_big,
                              by = names(prev_levels_grid_big[,-1]))

  # get levels / sim_ids that were previously run but are no longer needed
  extra_run <- dplyr::anti_join(prev_levels_grid_big[,-1],
                                levels_grid_big[,-1],
                                by = names(prev_levels_grid_big[,-1]))

  # if keep_extra = FALSE, remove excess runs (from results, errors, and warnings)
  if (!keep_extra & nrow(extra_run) > 0){

    if (!is.character(sim_obj$results)){
      sim_obj$results <- dplyr::anti_join(sim_obj$results,
                                          extra_run,
                                          by = names(extra_run))
    }
    if (!is.character(sim_obj$errors)){
      sim_obj$errors <- dplyr::anti_join(sim_obj$errors,
                                         extra_run,
                                         by = names(extra_run))
    }
    if (!is.character(sim_obj$warnings)){
      sim_obj$warnings <- dplyr::anti_join(sim_obj$warnings,
                                           extra_run,
                                           by = names(extra_run))
    }
  }

  # create a copy of the sim to hold new results
  sim_obj_copy <- sim_obj
  sim_obj_copy$internals$update <- TRUE
  #sim_obj_copy$internals$levels_grid_big <- levels_grid_big

  # if there are extra runs to do
  if (nrow(not_run) > 0){

    sim_obj_copy$internals$levels_grid_big <- not_run

    # for new run, do only levels / sim_ids that have not been run

    sim_obj_copy$results <- NULL
    sim_obj_copy$errors <- NULL
    sim_obj_copy$warnings <- NULL

    # run
    sim_obj_copy <- run(sim_obj_copy)

    # combine results/errors/warnings of original run and update
    if (!is.character(sim_obj_copy$results)){
      if (!is.character(sim_obj$results)){
        sim_obj_copy$results <- rbind(sim_obj$results, sim_obj_copy$results)
      }
    }
    else{
      sim_obj_copy$results <- sim_obj$results
    }
    if (!is.character(sim_obj_copy$errors)){
      if (!is.character(sim_obj$errors)){
        sim_obj_copy$errors <- rbind(sim_obj$errors, sim_obj_copy$errors)
      }
    }
    else{
      sim_obj_copy$errors <- sim_obj$errors
    }
    if (!is.character(sim_obj_copy$warnings)){
      if (!is.character(sim_obj$warnings)){
        sim_obj_copy$warnings <- rbind(sim_obj$warnings, sim_obj_copy$warnings)
      }
    }
    else{
      sim_obj_copy$warnings <- sim_obj$warnings
    }

  } else {
    warning("No additional simulations to run.")
  }

  sim_obj_copy$internals$levels_grid_big <- levels_grid_big

  return (sim_obj_copy)

}
