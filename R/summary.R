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

  # Parse/run summarize code
  args <- list(...)
  levels <- names(sim_results$levels)
  names_raw <- names(sim_results$raw)
  names_raw <- names_raw[!(names_raw %in% c(levels, "sim_uid",
                                            "sim_id", "level_id"))]
  code_levels <- paste0("'",levels,"'=",levels,"[1],")
  code_means <- paste0("'mean_",names_raw,"'=mean(",names_raw,"),")
  if (!is.null(args$sd) && args$sd==TRUE) {
    code_sds <- paste0("'sd_",names_raw,"'=sd(",names_raw,"),")
  } else {
    code_sds <- ""
  }
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(
      dplyr::group_by(sim_results$raw, level_id),",
      code_levels,
      code_means,
      code_sds
  )
  summarize_code <- c(summarize_code, "))")
  s <- eval(parse(text=summarize_code))

  summary <- list(
    "summary" = s
  ) # !!!!!

  class(summary) <- "simba_summary"

  return (summary)

}
