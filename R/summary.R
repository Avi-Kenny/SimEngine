#' Summarize simulation results
#'
#' @param sim_obj A simulation object created by new_sim()
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
  for (i in 1:length(sim_obj$constants)) {
    length <- nrow(R)
    R[[names(sim_obj$constants)[[i]]]] <- sim_obj$constants[[i]]
  }

  # Parse code to print levels and calculate means
  # !!!!! Options should be means=TRUE (default), means=FALSE, means=list()
  names_means <- names_results[!(names_results %in% c(
    names_levels, "sim_uid", "sim_id", "level_id"
  ))]
  code_levels <- paste0("'",names_levels,"'=",names_levels,"[1],")
  code_means <- paste0("'mean_",names_means,"'=mean(",names_means,"),")

  # If there is only one list, wrap it in a list
  # !!!!! Add 'mean' to this
  for (metric in c("sd", "bias", "coverage")) {
    if (!is.null(o_args[[metric]]) && !class(o_args[[metric]][[1]])=="list") {
      o_args[[metric]] <- list(o_args[[metric]])
    }
  }

  # !!!!! Also add variance

  # Parse SD summary code
  if (!is.null(o_args$sd)) {
    code_sd <- ""
    for (s in o_args$sd) {
      code_sd <- c(code_sd, paste0(
        s$name, " = sd(",s$x,"),"
      ))
    }
  } else {
    code_sd <- ""
  }

  # Calculate bias and parse coverage summary code
  if (!is.null(o_args$bias)) {
    code_bias <- ""
    for (b in o_args$bias) {
      code_bias <- c(code_bias, paste0(
        b$name, " = mean(",b$estimate,"-",b$truth,"),"
      ))
    }
  } else {
    code_bias <- ""
  }

  # Calculate CIs and parse coverage sumary code
  if (!is.null(o_args$coverage)) {

    code_cov <- ""
    for (cov in o_args$coverage) {

      ci_l <- R[[cov$estimate]] - 1.96*R[[cov$se]]
      ci_h <- R[[cov$estimate]] + 1.96*R[[cov$se]]
      R[[paste0(".ci_l_",cov$name)]] <- ci_l
      R[[paste0(".ci_h_",cov$name)]] <- ci_h

      code_cov <- c(code_cov, paste0(
        cov$name, " = mean(ifelse(.ci_l_", cov$name, " <= ", cov$truth,
        " & ", cov$truth, " <= .ci_h_", cov$name, ", 1, 0)),"
      ))

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
    code_bias,
    code_cov
  )
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))

  print(summary)

}
