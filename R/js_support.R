#' Display information about currently-supported job schedulers
#'
#' @description Run this function to display information about job schedulers
#'     that are currently supported for running \pkg{SimEngine} simulations on a
#'     cluster computing system (CCS).
#' @return NULL
#' @examples
#' js_support()
#' @export
js_support <- function() {

  df <- data.frame(
    "name" = character(),
    "js_code" = character(),
    "developer" = character(),
    "tid" = character(),
    "notes" = character(),
    stringsAsFactors = FALSE
  )

  add <- function(df, ls) {
    df[nrow(df)+1,] <- ls
    return(df)
  }

  df <- add(df, list(
    name = "Slurm workload manager",
    js_code = "slurm",
    developer = "SchedMD",
    tid = "SLURM_ARRAY_TASK_ID",
    notes = ""
  ))
  df <- add(df, list(
    name = "Oracle Grid Engine",
    js_code = "ge",
    developer = "Oracle",
    tid = "SGE_TASK_ID",
    notes = "Formerly known as 'Sun Grid Engine'"
  ))

  return(df)

}
