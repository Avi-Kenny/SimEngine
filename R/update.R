#' Update a simulation
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim(), that has already been run by run()
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
  if (!is.logical(keep_errors)){
    stop("'keep_errors' must be a logical")
  }
  if (!is.logical(keep_extra)){
    stop("'keep_extra' must be a logical")
  }

  # make sorted list of current levels and previous levels
  sorted_prev_levels <- sim_obj$internals$levels_prev[order(names(sim_obj$internals$levels_prev))]
  sorted_curr_levels <- sim_obj$internals$levels_shallow[order(names(sim_obj$internals$levels_shallow))]

  # disallow adding new level variables
  if (sum(names(sorted_prev_levels) != names(sorted_curr_levels)) != 0){
    stop("Updating a sim cannot include new level variables, only new levels.")
  }

  # Create levels_grid_big
  levels_grid_big <- expand.grid(list(
    "level_id" = sim_obj$levels_grid$level_id,
    "sim_id" = 1:sim_obj$config$num_sim
  ))

  levels_grid_big <- dplyr::inner_join(
    levels_grid_big,
    sim_obj$levels_grid,
    by = "level_id"
  )
  levels_grid_big <- dplyr::arrange(levels_grid_big, level_id, sim_id)
  names_2 <- names(levels_grid_big)
  levels_grid_big <- cbind(1:nrow(levels_grid_big), levels_grid_big)
  names(levels_grid_big) <- c("sim_uid", names_2)

  # make grid of previously run levels / sim_ids
  prev_levels_grid <- expand.grid(c(sorted_prev_levels,
                                    list("sim_id" = 1:sim_obj$internals$num_sim_prev)))
  if ("no levels" %in% names(sorted_prev_levels)){
    prev_levels_grid <- prev_levels_grid[,-which(names(prev_levels_grid) == "no levels"), drop = F]
  }

  # if re-running error reps, limit the prev_levels_grid to only those in results and revert the error df
  if (!keep_errors){
    prev_levels_grid <- dplyr::semi_join(prev_levels_grid,
                                         sim_obj$results,
                                         by = names(prev_levels_grid))
    sim_obj$errors <- "No errors"
  }

  # get levels / sim_ids that have not previously been run
  not_run <- dplyr::anti_join(levels_grid_big[,3:ncol(levels_grid_big), drop = F],
                              prev_levels_grid,
                              #by = c(names(sim_obj$internals$levels_prev), "sim_id"))
                              by = names(prev_levels_grid))

  # add sim_uids to not_run
  not_run <- dplyr::inner_join(levels_grid_big, not_run, by = names(prev_levels_grid))


  # get levels / sim_ids that were previously run but are no longer needed
  extra_run <- dplyr::anti_join(prev_levels_grid,
                                levels_grid_big[,3:ncol(levels_grid_big), drop = F],
                                by = names(prev_levels_grid))

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

  # if there are extra runs to do
  if (nrow(not_run) > 0){
    # for new run, do only levels / sim_ids that have not been run
    sim_obj_copy$internals$levels_grid_big <- not_run
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

  } else{
    warning("No additional simulations to run.")
  }

  # straighten out the uids
  if (!is.character(sim_obj_copy$results)){
    sim_obj_copy$results <- dplyr::inner_join(levels_grid_big,#[,c("sim_uid", "level_id", "sim_id")],
                                              sim_obj_copy$results[,-c(1,2)],
                                              by = names(prev_levels_grid))
  }
  if (!is.character(sim_obj_copy$errors)){
    sim_obj_copy$errors <- dplyr::inner_join(levels_grid_big,#[,c("sim_uid", "level_id", "sim_id")],
                                             sim_obj_copy$errors[,-c(1,2)],
                                             by = names(prev_levels_grid))
  }
  if (!is.character(sim_obj_copy$warnings)){
    sim_obj_copy$warnings <- dplyr::inner_join(levels_grid_big,#[,c("sim_uid", "level_id", "sim_id")],
                                               sim_obj_copy$warnings[,-c(1,2)],
                                               by = names(prev_levels_grid))
  }

  sim_obj_copy$internals$levels_grid_big <- levels_grid_big

  return (sim_obj_copy)

}
