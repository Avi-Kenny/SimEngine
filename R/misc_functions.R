# This section contains functions that are either intentionally not documented
#   or that are not exported

# Print method for class "simba"
#' @export
print.simba <- function(sim_obj) {
  cat("Simulation object (class \"simba\")\n")
  cat("---------------------------------\n")
  cat("Configuration: \n")
  for (i in 1:length(sim_obj$config)) {
    cat(paste0("    ",names(sim_obj$config)[i],": ",sim_obj$config[i],"\n"))
  }
  if (length(sim_obj$levels)>0) {
    cat("Levels: \n")
    for (i in 1:length(sim_obj$levels)) {
      cat(paste0("    ",names(sim_obj$levels)[i],": ",sim_obj$levels[i],"\n"))
    }
  }
  if (length(sim_obj$constants)>0) {
    cat("Constants: \n")
    for (i in 1:length(sim_obj$constants)) {
      cat(paste0("    ",names(sim_obj$constants)[i],"\n"))
    }
  }
  if (length(sim_obj$creators)>0) {
    cat("Creators: \n")
    for (i in 1:length(sim_obj$creators)) {
      cat(paste0("    ",names(sim_obj$creators)[i],"\n"))
    }
  }
  if (length(sim_obj$methods)>0) {
    cat("Methods: \n")
    for (i in 1:length(sim_obj$methods)) {
      cat(paste0("    ",names(sim_obj$methods)[i],"\n"))
    }
  }
  if (length(sim_obj$scripts)>0) {
    cat("Scripts: \n")
    for (i in 1:length(sim_obj$scripts)) {
      cat(paste0("    ",names(sim_obj$scripts)[i],"\n"))
    }
  }
}



#' Function for internal error handling
#'
#' @param obj The object to check for errors (can be a list)
#' @param err The type of error to check for (character string)
#' @param other A generic argument that can be used to pass in additional info
#' @return Throws and error or returns NULL
handle_errors <- function(obj, err, other=NA) {

  dso <- deparse(substitute(obj))

  switch(err,

    "is.simba" = {
      if (class(obj)!="simba") {
        stop(paste0("`",dso,"` must be of class `simba`"), call.=FALSE)
      }
    },

    "is.boolean" = {
      if (!(is.logical(obj) && length(obj) == 1)) {
        stop(paste0("`",dso,"` must be of type 'logical'"), call.=FALSE)
      }
    },

    "is.numeric" = {
      if (!(is.numeric(obj) && length(obj) == 1)) {
        stop(paste0("`",dso,"` must be numeric"), call.=FALSE)
      }
    },

    "is.in" = {
      if (length(obj)>1) {
        stop(paste0("`",dso,"` cannot be a vector"), call.=FALSE)
      } else if (!(obj %in% other)) {
        stop(paste0("'",obj,"' is not a valid option for `",dso,"`"),
             call.=FALSE)
      }
    },

    "is.function" = {
      if (!is.function(obj)) {
        stop(paste0("`",dso,"` must be a function"), call.=FALSE)
      }
    },

    "is.character" = {
      if (!(is.character(obj) && length(obj)==1)) {
        stop(paste0("`",dso,"` must be a character string"), call.=FALSE)
      }
    },

    "is.character.vector" = {
      if (!(is.character(obj))) {
        stop(paste0("`",dso,"` must be a character vector"), call.=FALSE)
      }
    },

    "error type" = {}

  )
}



#' # Print method for class "simba_results"
#' #' @export
#' print.simba_results <- function(results) {
#'   cat("Simulation results (raw):\n\n")
#'   print(results$raw)
#' }
#'
#' # Print method for class "simba_summary"
#' #' @export
#' print.simba_summary <- function(summary) {
#'   cat("Simulation results (summary across simulations):\n\n")
#'   print(summary$summary)
#' }
