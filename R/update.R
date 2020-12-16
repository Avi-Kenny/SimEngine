#' Update a simulation
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim(), that has already been run by run()
#' @param keep_errors Boolean (TRUE by default); if TRUE, do not try to re-run
#'     simulation reps that results in errors previously; if FALSE, attempt to
#'     run those reps again
#' @param keep_extra_reps Boolean (FALSE by default); if TRUE, keep previously run
#'     reps even if they exceed the current num_sim in config; if FALSE, randomly
#'     select excess reps to drop
#' @param keep_extra_levels Boolean (FALSE by default); if TRUE, keep previously run
#'     levels even if they are no longer included in current sim levels; if FALSE,
#'     drop them from results
#' @examples
#' !!!!! TO DO
#' @export
update <- function(sim_obj,
                   keep_errors = TRUE,
                   keep_extra_reps = FALSE,
                   keep_extra_levels = FALSE) UseMethod("update")

#' @export
update.simba <- function(sim_obj,
                         keep_errors = TRUE,
                         keep_extra_reps = FALSE,
                         keep_extra_levels = FALSE) {

  # !!!!! error handle invalid options

  # make sorted list of current levels and previous levels
  sorted_prev_levels <- sim_obj$internals$levels_prev[order(names(sim_obj$internals$levels_prev))]
  sorted_curr_levels <- sim_obj$internals$levels_shallow[order(names(sim_obj$internals$levels_shallow))]

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

  if (!keep_errors){
    prev_levels_grid <- dplyr::semi_join(prev_levels_grid,
                                         sim_obj$results,
                                         by = c(names(sim_obj$internals$levels_prev), "sim_id"))
    sim_obj$errors <- NULL
  }

  # get levels / sim_ids that have not previously been run
  not_run <- dplyr::anti_join(levels_grid_big[,3:ncol(levels_grid_big)],
                              prev_levels_grid,
                              by = c(names(sim_obj$internals$levels_prev), "sim_id"))

  not_run <- dplyr::inner_join(levels_grid_big, not_run, by = c(names(sim_obj$internals$levels_prev), "sim_id"))

  if (nrow(not_run) == 0){
    stop("No additional simulations to run.")
  }

  # for new run, do only levels / sim_ids that have not been run
  sim_obj$internals$levels_grid_big <- not_run

  sim_obj_copy <- sim_obj
  sim_obj_copy$results <- NULL
  sim_obj_copy$errors <- NULL
  sim_obj_copy$warnings <- NULL
  sim_obj_copy$internals$update <- TRUE

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

  sim_obj_copy$results <- dplyr::inner_join(levels_grid_big[,c("sim_uid", "level_id", "sim_id")],
                                            sim_obj_copy$results[,-1],
                                            by = c("level_id", "sim_id"))

  return (sim_obj_copy)

}
