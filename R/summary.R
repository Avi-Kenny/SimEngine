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
  # !!!!! Note, if user has referenced any variables NOT through C, R, or L, this will lead to an error
  L <- sim_obj$levels
  C <- sim_obj$constants
  R <- as.list(sim_obj$results)

  eval(parse(text=c("o_args <- ", deparse(substitute(list(...))))))

  # Parse code to print levels and calculate means
  L_names <- names(L)
  names_raw <- names(sim_obj$results)
  names_raw <- names_raw[!(names_raw %in% c(L_names, "sim_uid",
                                            "sim_id", "level_id"))]
  code_levels <- paste0("'",L_names,"'=",L_names,"[1],")
  code_means <- paste0("'mean_",names_raw,"'=mean(",names_raw,"),")

  # Parse code to calculate SDs
  if (!is.null(o_args$sd) && o_args$sd==TRUE) {
    code_sds <- paste0("'sd_",names_raw,"'=sd(",names_raw,"),")
  } else {
    code_sds <- ""
  }

  # If there is only one list (i.e. one coverage indicator), warp it in a list
  if (!is.null(o_args$coverage$name)) {
    o_args$coverage <- list(o_args$coverage)
  }

  # Calculate CIs/coverage
  if (!is.null(o_args$coverage)) {

    for (cov in o_args$coverage) {
      ci_l <- cov$estimate - 1.96*cov$se
      ci_h <- cov$estimate + 1.96*cov$se
      sim_obj$results[[paste0("ci_l_",cov$name)]] <- ci_l
      sim_obj$results[[paste0("ci_h_",cov$name)]] <- ci_h
    }

  }

  # Parse code to calculate coverage
  if (!is.null(o_args$coverage)) {

    code_cov <- ""
    for (cov in o_args$coverage) {
      code_cov <- c(code_cov, paste0(
        "cov_", cov$name, " = mean(ifelse(ci_l_", cov$name, " <= ",
        cov$truth, " & ", cov$truth, " <= ci_h_", cov$name, ", 1, 0)),"
      ))
    }

  } else {
    code_cov <- ""
  }

  # Put code strings together
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(
       dplyr::group_by(sim_obj$results, level_id),",
    code_levels,
    code_means,
    code_sds,
    code_cov
  )
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))

  print(summary)

}
