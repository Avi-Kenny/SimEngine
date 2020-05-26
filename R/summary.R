#' Summarize simulation results
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param sd If `sd=TRUE` is passed, standard deviations are reported in
#'     addition to means
#' @param coverage !!!!! TO DO
#' @return !!!!! TO DO
#' @examples
#' !!!!! TO DO
#' @export
summary.simba <- function(sim_obj, ...) {

  if (is.null(sim_obj$results)) {
    stop("Simulation has not been run yet")
  }

  # Parse passed arguments
  R <- sim_obj$results
  names_levels <- names(sim_obj$levels)
  names_results <- names(sim_obj$results)

  # Evaluate passed arguments, passing in constants
  o_args <- list(...)
  # eval(parse(text=c("o_args <- ", deparse(substitute(list(...))))))

  # Add simulation constants to `R` data frame
  # !!!!! This is problematic if constants are not numbers, strings, etc.
  # !!!!! Temp fix: declare constants in current environment
  # if (length(sim_obj$constants)!=0) {
  #   for (i in 1:length(sim_obj$constants)) {
  #     length <- nrow(R)
  #     R[[names(sim_obj$constants)[[i]]]] <- sim_obj$constants[[i]]
  #   }
  # }
  if (length(sim_obj$constants)!=0) {
    for (i in 1:length(sim_obj$constants)) {
      assign(
        x = names(sim_obj$constants)[i],
        value = (sim_obj$constants)[[i]]
      )
    }
  }


  # If there is only one list, wrap it in a list
  # !!!!! Add 'mean' to this
  for (metric in c("mean", "sd", "bias", "coverage")) {
    if (!is.null(o_args[[metric]]) && !class(o_args[[metric]][[1]])=="list") {
      o_args[[metric]] <- list(o_args[[metric]])
    }
  }

  # Parse code to display levels
  code_levels <- paste0("'",names_levels,"'=",names_levels,"[1],")

  # Parse code to print levels and calculate means
  # !!!!! Options should be means=TRUE (default), means=FALSE, means=list()
  # !!!!! Add na.rm

  if (is.null(o_args$means) ||
      (!is.null(o_args$means$all) && o_args$means$all==TRUE) ) {

    if (!is.null(o_args$means$na.rm) && o_args$means$na.rm==TRUE) {
      na_1 <- ", na.rm=TRUE),"
    } else {
      na_1 <- "),"
    }

    names_means <- names_results[!(names_results %in% c(
      names_levels, "sim_uid", "sim_id", "level_id"
    ))]
    code_means <- paste0("'mean_", names_means, "'=mean(", names_means, na_1)

  } else {

    # !!!!! TO DO

  }

  # Parse SD summary code
  if (!is.null(o_args$sd)) { # !!!!! be consistent about o_args$sd vs o_args[["sd"]]

    code_sd <- ""
    for (sd in o_args$sd) {

      if (!is.null(sd$na.rm) && sd$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_sd <- c(code_sd, paste0(
        sd$name, " = sd(", sd$x, na_1
      ))

    }
  } else {
    code_sd <- ""
  }

  # Parse variance summary code
  if (!is.null(o_args$var)) {

    code_var <- ""
    for (var in o_args$var) {

      if (!is.null(var$na.rm) && var$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_var <- c(code_var, paste0(
        var$name, " = var(", var$x, na_1
      ))

    }
  } else {
    code_var <- ""
  }

  # Calculate bias and parse summary code
  if (!is.null(o_args$bias)) {

    code_bias <- ""
    for (b in o_args$bias) {

      if (!is.null(b$na.rm) && b$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_bias <- c(code_bias, paste0(
        b$name, " = mean(", b$estimate, "-", b$truth, na_1
      ))

    }

  } else {
    code_bias <- ""
  }

  # Calculate CIs and parse coverage sumary code
  # !!!!! Add a column to specify how many rows were omitted with na.rm (for other summary stats as well)
  if (!is.null(o_args$coverage)) {

    code_cov <- ""
    for (cov in o_args$coverage) {

      ci_l <- R[[cov$estimate]] - 1.96*R[[cov$se]]
      ci_h <- R[[cov$estimate]] + 1.96*R[[cov$se]]
      R[[paste0(".ci_l_",cov$name)]] <- ci_l
      R[[paste0(".ci_h_",cov$name)]] <- ci_h

      if (!is.null(cov$na.rm) && cov$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
        na_2 <- paste0(cov$name, "_num_na", " = sum(is.na(", cov$estimate,
                       ") | is.na(", cov$se,")),")
      } else {
        na_1 <- "),"
        na_2 <- ""
      }

      code_cov <- c(code_cov, paste0(
        cov$name, " = mean(ifelse(.ci_l_", cov$name, " <= ", cov$truth,
        " & ", cov$truth, " <= .ci_h_", cov$name, ", 1, 0)", na_1, na_2))

    }

  } else {
    code_cov <- ""
  }

  # Put code strings together
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(
       dplyr::group_by(R, level_id),",
    code_levels,
    code_means,
    code_sd,
    code_var,
    code_bias,
    code_cov
  )
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))

  return (summary)

}
