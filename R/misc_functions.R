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
      if (!is.null(obj) && is.na(obj)) {
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

  sim_uid_grid_new <- expand.grid(list(
    "level_id" = sim$levels_grid$level_id,
    "rep_id" = c(1:sim$config$num_sim)
  ))
  sim_uid_grid_new %<>% dplyr::arrange(.data$level_id, .data$rep_id)
  sim_uid_grid_new$active <- T

  if (attr(sim$internals$sim_uid_grid, "blank")) {

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
    inds <- which(is.na(sim_uid_grid$active))
    sim_uid_grid$active[inds] <- F
    sim_uid_grid$to_run[inds] <- F

    # Remove old batch_id and core_id columns
    sim_uid_grid$batch_id <- NULL
    sim_uid_grid$core_id <- NULL

    # Subset sim_uid_grid_new to new sim_uids
    sim_uid_grid_new <- dplyr::anti_join(
      sim_uid_grid_new,
      sim_uid_grid[,c("level_id", "rep_id")],
      by = c("level_id", "rep_id")
    )

    if (nrow(sim_uid_grid_new)>0) {

      sim_uid_grid_new$to_run <- T

      # Add new sim_uids to sim_uid_grid_new
      max_uid <- max(sim_uid_grid$sim_uid)
      sim_uid_grid_new <- cbind(
        sim_uid = max_uid + c(1:nrow(sim_uid_grid_new)),
        sim_uid_grid_new
      )

      # Add sim_uid_grid_new to sim_uid_grid
      sim_uid_grid_new %<>% dplyr::relocate(.data$active, .after=.data$to_run)
      sim_uid_grid <- rbind(sim_uid_grid, sim_uid_grid_new)

    }

  }

  sims_to_run <- which(sim_uid_grid$to_run)

  if (sim$vars$run_state=="pre run") {

    # Add batch_id column
    sim_uid_grid <- dplyr::left_join(
      sim_uid_grid,
      sim$internals$level_batch_map,
      by = "level_id"
    )
    sim_uid_grid$batch_id <- as.integer(as.factor(paste0(
      sim_uid_grid$batch_id, "-", sim_uid_grid$rep_id
    )))
    sim_uid_grid$batch_id_pre <- NULL

  } else {

    # Create placeholder/unused batch_id column
    batch_id <- rep(0, nrow(sim_uid_grid))
    batch_id[which(sim_uid_grid$to_run)] <- c(1:sum(sim_uid_grid$to_run))
    sim_uid_grid$batch_id <- batch_id

  }

  # Create new core_id column
  nc <- sim$config$n_cores
  sim_uid_grid$core_id <- 0
  if (is.na(nc) && Sys.getenv("sim_run")!="") {
    sim_uid_grid$core_id[sims_to_run] <- c(1:length(sims_to_run))
  } else {
    if (is.na(nc)) { nc <- 1 }
    sim_uid_grid$core_id[sims_to_run] <-
      ((sim_uid_grid$batch_id[sims_to_run]-1)%%nc)+1
  }

  # Make sure there are no reps with active==F and to_run==T
  if (any(sim_uid_grid$to_run &
          !sim_uid_grid$active)) {
    stop("An unknown error occurred (CODE 103)")
  }

  attr(sim_uid_grid, "blank") <- F

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
    if (length(sim$results_complex)>0) {
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

  if (is.character(sim$errors)) {
    return("run, no errors")
  } else if (is.character(sim$results)) {
    return("run, all errors")
  } else if (nrow(sim$results>0) && nrow(sim$errors>0)) {
    return("run, some errors")
  } else {
    stop("An unknown error occurred (CODE 105)")
  }

}



#' Delete results/warnings/errors corresponding to inactive sim_uids
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return A simulation object with inactive results/warnings/errors deleted
#' @noRd
delete_inactive_rwe <- function(sim) {

  inactive_uids <- sim$internals$sim_uid_grid$sim_uid[
    which(!sim$internals$sim_uid_grid$active)
  ]

  if (length(inactive_uids)>0) {
    if (!is.character(sim$results)) {
      sim$results <- sim$results[
        which(!(sim$results$sim_uid %in% inactive_uids)),
      ]
    }
    if (!is.character(sim$errors)) {
      sim$errors <- sim$errors[
        which(!(sim$errors$sim_uid %in% inactive_uids)),
      ]
    }
    if (!is.character(sim$warnings)) {
      sim$warnings <- sim$warnings[
        which(!(sim$warnings$sim_uid %in% inactive_uids)),
      ]
    }
    for (sim_uid in inactive_uids) {
      sim$results_complex[[paste0("sim_uid_",sim_uid)]] <- NULL
    }
  }

  return(sim)

}



#' Create or update level_batch_map association table
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @return An association table containing columns level_id and batch_id_pre; to
#'     add to sim$internals$level_batch_map
#' @noRd
update_level_batch_map <- function(sim) {

  if (!is.null(sim$config$batch_levels) && is.na(sim$config$batch_levels[1])) {
    batch_id_pre <- c(1:nrow(sim$levels_grid))
  } else if (is.null(sim$config$batch_levels)) {
    batch_id_pre <- rep(1,nrow(sim$levels_grid))
  } else {
    keys <- new.env()
    batch_id_pre <- rep(NA, nrow(sim$levels_grid))
    counter <- 1
    for (i in c(1:nrow(sim$levels_grid))) {
      key <- paste(unlist(lapply(sim$config$batch_levels, function(key) {
        paste0(key, "=", sim$levels_grid[i,key])
      })), collapse=";")
      if (is.null(keys[[key]])) {
        keys[[key]] <- counter
        batch_id_pre[i] <- counter
        counter <- counter + 1
      } else {
        batch_id_pre[i] <- keys[[key]]
      }
    }
  }

  return(data.frame(
    level_id = sim$levels_grid$level_id,
    batch_id_pre = batch_id_pre
  ))

}
