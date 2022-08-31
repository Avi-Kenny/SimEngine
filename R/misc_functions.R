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

    "is.sim_obj" = {
      if (!methods::is(obj,"sim_obj")) {
        if (is.na(msg)) {
          msg <- paste0("`",name,"` must be of class `sim_obj`")
        }
        stop(msg, call.=FALSE)
      }
    },

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

    "error type" = {}

  )
}

#' Function for creating levels_grid_big
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @importFrom rlang .data
#' @importFrom stats runif
#' @noRd
create_levels_grid_big <- function(sim) {

  # Add once_id to levels_grid
  if (!is.null(sim$config$once)) {
    keys <- new.env()
    once_ids <- rep(NA, nrow(sim$levels_grid))
    counter <- 1
    for (i in c(1:nrow(sim$levels_grid))) {
      key <- paste(unlist(lapply(sim$config$once, function(key) {
        paste0(key, "=", sim$levels_grid[i,key])
      })), collapse=";")
      if (is.null(keys[[key]])) {
        keys[[key]] <- counter
        once_ids[i] <- counter
        counter <- counter + 1
      } else {
        once_ids[i] <- keys[[key]]
      }
    }
    sim$levels_grid$once_id <- once_ids
  }

  # Create expanded levels grid, one row per replicate
  levels_grid_big <- expand.grid(list(
    "level_id" = sim$levels_grid$level_id,
    "rep_id" = 1:sim$config$num_sim
  ))
  levels_grid_big <- dplyr::inner_join(
    levels_grid_big,
    sim$levels_grid,
    by = "level_id"
  )
  levels_grid_big <- dplyr::arrange(levels_grid_big, .data$level_id,
                                    .data$rep_id)

  # Create sim_uid
  names_2 <- names(levels_grid_big)
  levels_grid_big <- cbind(1:nrow(levels_grid_big), levels_grid_big)
  names(levels_grid_big) <- c("sim_uid", names_2)

  # Update once_id
  if (!is.null(sim$config$once)) {
    levels_grid_big$once_id <- as.integer(as.factor(paste0(
      levels_grid_big$rep_id, "-", levels_grid_big$once_id
    )))
  }

  return(levels_grid_big)
}
