#' Summarize simulation results
#'
#' @param sim_results A simulation results object created by run()
#' @param sd If `sd=TRUE` is passed, standard deviations are reported in
#'     addition to means
#' @return !!!!! TO DO
#' @examples
#' !!!!! TO DO
#' @export
summary.simba_results <- function(sim_results, ...) {

  # Parse passed arguments
  # !!!!! Note, if user has referenced any variables NOT through C, R, or L, this will lead to an error
  L <- sim_results$levels
  C <- sim_results$constants
  R <- as.list(results$raw)

  eval(parse(text=c("o_args <- ", deparse(substitute(list(...))))))

  # Parse code to print levels and calculate means
  L_names <- names(L)
  names_raw <- names(sim_results$raw)
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
      sim_results$raw[[paste0("ci_l_",cov$name)]] <- ci_l
      sim_results$raw[[paste0("ci_h_",cov$name)]] <- ci_h
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
  # !!!!! TO DO

  # Put code strings together
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(
       dplyr::group_by(sim_results$raw, level_id),",
    code_levels,
    code_means,
    code_sds,
    code_cov
  )
  summarize_code <- c(summarize_code, "))")
  s <- eval(parse(text=summarize_code))

  summary <- list("summary" = s)

  class(summary) <- "simba_summary"

  return (summary)

}
