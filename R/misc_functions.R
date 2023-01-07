# This section contains functions that are either intentionally not documented
#   or that are not exported



#' Print method for class \code{sim_obj}
#' @noRd
#' @export
print.sim_obj <- function(x, ...) {
  sim <- x
  cat("SimEngine: simulation object (class \"sim_obj\")\n")
  cat("----------------------------------------------\n")
  cat("Configuration: \n")
  for (i in 1:length(sim$config)) {
    cat(paste0("    ",names(sim$config)[i],": ",sim$config[i],"\n"))
  }
  if (length(sim$levels)>0) {
    cat("Levels: \n")
    for (i in 1:length(sim$levels)) {
      cat(paste0("    ",names(sim$levels)[i],": ",sim$levels[i],"\n"))
    }
  }
  if (length(sim$scripts)>0) {
    cat("Scripts: \n")
    for (i in 1:length(sim$scripts)) {
      cat(paste0("    ",names(sim$scripts)[i],"\n"))
    }
  }
  cat(paste0("State: ",sim$vars$run_state,"\n"))
}



#' Function for internal error handling
#'
#' @param obj The object to check for errors (can be a list)
#' @param err The type of error to check for (character string)
#' @param name Name of the object; if not provided, the name of the obj variable
#'     is used
#' @param other A generic argument that can be used to pass in additional info
#' @param msg A custom error message
#' @return Throws and error or returns NULL
#' @noRd
handle_errors <- function(obj, err, name=NA, other=NA, msg=NA) {

  if (is.na(name)) { name <- deparse(substitute(obj)) }

  # Throw error (without function call) if obj is not found
  tryCatch(.e <- obj, error = function(e) {
    msg <- e$message
    stop(msg, call.=FALSE)
  })

  switch(err,

    "is.boolean" = {
      if (!(is.logical(obj) && length(obj) == 1)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be of type 'logical', of length 1")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.numeric" = {
      if (!(is.numeric(obj) && length(obj) == 1)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be numeric, of length 1")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.numeric.vec" = {
      if (!(is.numeric(obj))) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be numeric")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.in" = {
      if (length(obj)>1) {
        stop(paste0("`",name,"` cannot be a vector"), call.=FALSE)
      } else if (!(obj %in% other)) {
        if (is.na(msg)) {
          msg <- paste0("'",obj,"' is not a valid option for `",name,"`")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.function" = {
      if (!is.function(obj)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be a function")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.character" = {
      if (!(is.character(obj) && length(obj)==1)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be a character string, of length 1")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.character.vec" = {
      if (!(is.character(obj))) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be a character vector")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.null" = {
      if (is.null(obj)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must not be NULL")
        }
        stop(msg, call.=FALSE)
      }
    },

    "is.na" = {
      if (is.na(obj)) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must not be NA")
        }
        stop(msg, call.=FALSE)
      }
    },

    "error type" = {}

  )
}



#' Function for creating or updating sim_uid_grid
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return sim_uid_grid dataframe
#' @noRd
update_sim_uid_grid <- function(sim) {

  # !!!!! Make sure this handles sims with no levels
  # !!!!! Maybe take a second 'update' argument

  sim_uid_grid_new <- expand.grid(list(
    "level_id" = sim$levels_grid$level_id,
    "rep_id" = c(1:sim$config$num_sim)
  ))
  sim_uid_grid_new %<>% dplyr::arrange(level_id, rep_id)
  sim_uid_grid_new$active <- T

  if (nrow(sim$internals$sim_uid_grid)==0) {

    sim_uid_grid <- cbind(
      sim_uid = c(1:nrow(sim_uid_grid_new)),
      sim_uid_grid_new
    )
    sim_uid_grid$to_run <- T

  } else {

    # Mark removed sim_uids as active=F
    sim_uid_grid <- sim$internals$sim_uid_grid
    sim_uid_grid <- dplyr::left_join(
      sim_uid_grid[,-which(names(sim_uid_grid)=="active")],
      sim_uid_grid_new,
      by = c("level_id", "rep_id")
    )
    sim_uid_grid$active[which(is.na(sim_uid_grid$active))] <- F

    # Subset sim_uid_grid_new to new sim_uids
    sim_uid_grid_new <- dplyr::anti_join(
      sim_uid_grid_new,
      sim_uid_grid[,c("level_id", "rep_id")],
      by = c("level_id", "rep_id")
    )
    sim_uid_grid_new$to_run <- T

    # Add new sim_uids to sim_uid_grid_new
    max_uid <- max(sim_uid_grid$sim_uid)
    sim_uid_grid_new <- cbind(
      sim_uid = max_uid + c(1:nrow(sim_uid_grid_new)),
      sim_uid_grid_new
    )

    # Remove old batch_id2 and core_id columns
    sim_uid_grid$batch_id2 <- NULL
    sim_uid_grid$core_id <- NULL

    # Add sim_uid_grid_new to sim_uid_grid
    sim_uid_grid_new %<>% dplyr::relocate(active, .after=to_run)
    sim_uid_grid <- rbind(sim_uid_grid, sim_uid_grid_new)

  }

  sims_to_run <- which(sim_uid_grid$to_run)

  if (sim$vars$run_state=="pre run") {

    # Add batch_id2 column
    sim_uid_grid <- dplyr::left_join(
      sim_uid_grid,
      sim$levels_grid[,c("level_id", "batch_id")],
      by = "level_id"
    )
    sim_uid_grid %<>% dplyr::rename("batch_id2" = batch_id)
    sim_uid_grid$batch_id2 <- as.integer(as.factor(paste0(
      sim_uid_grid$batch_id2, "-", sim_uid_grid$rep_id
    )))

  } else {

    # Create placeholder batch_id2 column (unused)
    batch_id2 <- rep(0, nrow(sim_uid_grid))
    batch_id2[which(sim_uid_grid$to_run)] <- c(1:sum(sim_uid_grid$to_run))

  }

  # Create new core_id column
  nc <- sim$config$n_cores
  if (is.na(nc)) { nc <- 1 }
  sim_uid_grid$core_id <- 0
  sim_uid_grid$core_id[sims_to_run] <-
    ((sim_uid_grid$batch_id2[sims_to_run]-1)%%nc)+1

  return(sim_uid_grid)

}



#' Calculate num_sim_total
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return An integer representing how many replicates need to run
#' @note This may be incorrect if keep_errors=F
#' @noRd
num_sim_total <- function(sim) {

  return(sum(sim$internals$sim_uid_grid$to_run))

  # if (sim$vars$run_state=="pre run") {
  #   return(nrow(sim$levels_grid) * sim$config$num_sim)
  # } else {
  #   return(update_levels_grids(sim)$uids_to_run)
  # }

}



#' Calculate the sim$vars$run_state variable
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return A character string representing the "run state"
#' @noRd
combine_original_with_update <- function(
  sim, results_new, results_complex_new, errors_new, warnings_new
) {

  if (!is.null(results_new)) {
    if (!is.character(sim$results)) {
      sim$results <- rbind(sim$results, results_new)
    } else {
      sim$results <- results_new
    }
    if (!is.na(sim$results_complex)) {
      sim$results_complex <- c(sim$results_complex, results_complex_new)
    } else {
      sim$results_complex <- results_complex_new
    }
  }
  if (!is.null(errors_new)) {
    if (!is.character(sim$errors)) {
      sim$errors <- rbind(sim$errors, errors_new)
    } else {
      sim$errors <- errors_new
    }
  }
  if (!is.null(warnings_new)) {
    if (!is.character(sim$warnings)) {
      sim$warnings <- rbind(sim$warnings, warnings_new)
    } else {
      sim$warnings <- warnings_new
    }
  }

  return(sim)

}



#' Calculate the sim$vars$run_state variable
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return A character string representing the "run state"
#' @noRd
update_run_state <- function(sim) {

  all_errors <- "Errors detected in 100% of simulation replicates"

  if (sim$errors=="No errors") {
    return("run, no errors")
  } else if (sim$results==all_errors) {
    return("run, all errors")
  } else if (nrow(sim$results>0) && nrow(sim$errors>0)) {
    return("run, some errors")
  } else {
    stop("An unknown error occurred (CODE 102)")
  }

}



#' Delete results/warnings/errors corresponding to inactive sim_uids
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return A simulation object with inactive results/warnings/errors deleted
#' @noRd
delete_inactive_rwe <- function(sim) {

  inactive_uids <- dplyr::filter(sim$internals$sim_uid_grid, !active)$sim_uid

  if (length(inactive_uids)>0) {
    if (!is.character(sim$results)) {
      sim$results %<>% dplyr::filter(!(sim_uid %in% inactive_uids))
    }
    if (!is.character(sim$errors)) {
      sim$errors %<>% dplyr::filter(!(sim_uid %in% inactive_uids))
    }
    if (!is.character(sim$warnings)) {
      sim$warnings %<>% dplyr::filter(!(sim_uid %in% inactive_uids))
    }
    for (sim_uid in inactive_uids) {
      sim$results_complex[[paste0("sim_uid_",sim_uid)]] <- NULL
    }
  }

  return(sim)

}


#' Update batch_ids
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return A vector of batch_ids to add to sim$levels_grid
#' @noRd
update_batch_ids <- function(sim) {

  # !!!!! Make sure this works with update=T

  if (is.na(sim$config$batch_levels[1])) {
    batch_ids <- c(1:nrow(sim$levels_grid))
  } else {
    keys <- new.env()
    batch_ids <- rep(NA, nrow(sim$levels_grid))
    counter <- 1
    for (i in c(1:nrow(sim$levels_grid))) {
      key <- paste(unlist(lapply(sim$config$batch_levels, function(key) {
        paste0(key, "=", sim$levels_grid[i,key])
      })), collapse=";")
      if (is.null(keys[[key]])) {
        keys[[key]] <- counter
        batch_ids[i] <- counter
        counter <- counter + 1
      } else {
        batch_ids[i] <- keys[[key]]
      }
    }
  }

  return(batch_ids)

}
