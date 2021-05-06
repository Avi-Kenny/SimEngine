#' Summarize simulation results
#'
#' @description This function calculates summary statistics for simulation results.
#'     Options for summary statistics include descriptive statistics (e.g. measures of
#'     center or spread) and inferential statistics (e.g. bias or confidence interval
#'     coverage). All summary statistics are calculated over simulation replicates
#'     within a single simulation level.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param ... Name-value pairs of summary functions. The possible summary functions (names) are
#'     listed below. The value for each summary function is a list of summaries to perform.
#'     \itemize{
#'     \item{\code{mean}: Each \code{mean} summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the mean, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{median}: Each \code{median} summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the median, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{var}: Each \code{var} (variance) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the variance, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{sd}: Each \code{sd} (standard deviation) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the standard deviation, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{mad}: Each \code{mad} (mean absolute deviation) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the MAD, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{iqr}: Each \code{iqr} (interquartile range) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the IQR, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{min}: Each \code{min} (minimum) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the minimum, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{max}: Each \code{max} (maximum) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the maximum, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{quantile}: Each \code{quantile} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim_obj$results}
#'     on which to calculate the quantile, \code{prob} is a number in [0,1] denoting the desired quantile,
#'     and \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{bias}: Each \code{bias} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim_obj$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{bias_pct}: Each \code{bias_pct} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim_obj$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{mse}: Each \code{mse} (mean squared error) summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim_obj$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{mae}: Each \code{mae} (mean absolute error) summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim_obj$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{cov}: Each \code{cov} (confidence interval coverage) summary is a named list of five arguments. Either
#'     (\code{estimate}, \code{se}) or (\code{lower}, \code{upper}) must be provided.  \code{name} gives a name for the
#'     summary, \code{estimate} gives the name of the variable in \code{sim_obj$results}
#'     containing the estimator of interest, \code{se} gives the name of the variable in \code{sim_obj$results}
#'     containing the standard error of the estimator of interest, \code{lower} gives the name of the variable in
#'     \code{sim_obj$results} containing the confidence interval lower bound, \code{upper} gives the name of the
#'     variable in \code{sim_obj$results} containing the confidence interval upper bound, \code{truth} is the
#'     estimand of interest, and \code{na.rm} indicates whether to exclude \code{NA} values when performing the
#'     calculation. See \emph{Details}.}
#'    }
#' @details \itemize{
#'     \item{For all summaries besides \code{cov}, the \code{name} argument is optional. If \code{name} is not provided,
#'     a name will be formed from the type of summary and the column on which the summary is performed.}
#'
#'     \item{For all inferential summaries there are three ways to specify \code{truth}: (1) a single number,
#'     meaning the estimand is the same across all simulation replicates and levels, (2) a numeric vector of the
#'     same length as the number of rows in \code{sim_obj$results}, or (3) the name of a variable in \code{sim_obj$results}
#'     containing the estimand of interest.}
#'
#'     \item{There are two ways to specify the confidence interval bounds for \code{cov}. The first is to provide
#'     an \code{estimate} and its associated \code{se} (standard error). These should both be variables in
#'     \code{sim_obj$results}. The function constructs a 95\% Wald-type confidence interval of the form
#'     (\code{estimate} - 1.96 \code{se}, \code{estimate} + 1.96 \code{se}).  The alternative is to provide
#'     \code{lower} and \code{upper} bounds, which should also be variables in \code{sim_obj$results}. In this case,
#'     the confidence interval is (\code{lower}, \code{upper}). The coverage is simply the proportion of simulation
#'     replicates for a given level in which \code{truth} lies within the interval.}
#' }
#' @return A data frame containing the result of each specified summary function as a column, for each of
#'     the simulation levels.
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the summary function.
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#' sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
#' sim %<>% add_method("estimator_2", function(dat) { var(dat) })
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "estimator" = c("estimator_1", "estimator_2")
#' )
#' sim %<>% set_config(num_sim=5)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- do.call(L$estimator, list(dat))
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim %>% summary(
#'   mean = list(name="mean_lambda_hat", x="lambda_hat"),
#'   mse = list(name="lambda_mse", estimate="lambda_hat", truth=5)
#' )
#' @export
summary <- function(sim_obj, ...) UseMethod("summary")

#' @export
summary.simba <- function(sim_obj, ...) {

  handle_errors(sim_obj, "is.simba")

  # handle scenarios with no results
  if (sim_obj$internals$run_state == "pre run"){
    stop("Simulation has not been run yet.")
  }
  if (sim_obj$internals$run_state == "run, all errors"){
    stop("100% of simulations had errors.")
  }

  # Parse passed arguments
  R <- sim_obj$results
  names_levels <- names(sim_obj$levels)
  names_results <- names(sim_obj$results)

  # Evaluate passed arguments, passing in constants
  o_args <- list(...)

  # If no additional arguments provided to summary, display means by default
  if (identical(o_args,list())) {

    names_means <- names_results[!(names_results %in% c(
      names_levels, "sim_uid", "sim_id", "level_id"
    ))]

    o_args <- list()
    for (i in 1:length(names_means)) {
      o_args[[i]] <- list(name=paste0("mean_",names_means[i]), x=names_means[i])
    }
    o_args <- list(mean=o_args)

  }

  # If there is only one list, wrap it in a list
  metrics <- c("mean",
               "median",
               "var",
               "sd",
               "mad",
               "iqr",
               "quantile",
               "min",
               "max",
               "bias",
               "bias_pct",
               "mse",
               "mae",
               "coverage")
  for (arg_name in names(o_args)){
    if (!(arg_name %in% metrics)){
      stop(paste0(arg_name, " is an invalid summary metric."))
    }
  }
  for (metric in metrics) {
    if (!is.null(o_args[[metric]]) && !class(o_args[[metric]][[1]])=="list") {
      o_args[[metric]] <- list(o_args[[metric]])
    }
  }

  # Parse code to display levels
  if (is.null(sim_obj$levels$`no levels`)) {
    code_levels <- paste0("'",names_levels,"'=`",names_levels,"`[1],")
  } else {
    code_levels <- ""
  }

  ### Parse code to calculate mean
  if (!is.null(o_args$mean)) {

    code_mean <- ""
    for (m in o_args$mean) {

      # handle missing x argument
      if (is.null(m$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("mean_", m$x)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(m$x %in% names(R))){
        stop(paste0(m$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[m$x]])){
        stop(paste0(m$x, " is not numeric."))
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mean <- c(code_mean, paste0(
        m$name, " = mean(", m$x, na_1
      ))

    }
  } else {
    code_mean <- ""
  }

  ### Parse SD summary code
  if (!is.null(o_args$sd)) { # !!!!! be consistent about o_args$sd vs o_args[["sd"]]

    code_sd <- ""
    for (sd in o_args$sd) {

      # handle missing x argument
      if (is.null(sd$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(sd$name)){
        sd$name <- paste0("sd_", sd$x)
      }

      if (!is.character(sd$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(sd$x %in% names(R))){
        stop(paste0(sd$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[sd$x]])){
        stop(paste0(sd$x, " is not numeric."))
      }

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

  ### Parse variance summary code
  if (!is.null(o_args$var)) {

    code_var <- ""
    for (var in o_args$var) {

      # handle missing x argument
      if (is.null(var$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(var$name)){
        var$name <- paste0("var_", var$x)
      }

      if (!is.character(var$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(var$x %in% names(R))){
        stop(paste0(var$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[var$x]])){
        stop(paste0(var$x, " is not numeric."))
      }


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


  ### Parse MAD summary code
  if (!is.null(o_args$mad)) {

    code_mad <- ""
    for (m in o_args$mad) {

      # handle missing x argument
      if (is.null(m$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MAD_", m$x)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(m$x %in% names(R))){
        stop(paste0(m$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[m$x]])){
        stop(paste0(m$x, " is not numeric."))
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mad <- c(code_mad, paste0(
        m$name, " = mad(", m$x, na_1
      ))

    }
  } else {
    code_mad <- ""
  }


  ### Parse IQR summary code
  if (!is.null(o_args$iqr)) {

    code_iqr <- ""
    for (i in o_args$iqr) {

      # handle missing x argument
      if (is.null(i$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(i$name)){
        i$name <- paste0("IQR_", i$x)
      }

      if (!is.character(i$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(i$x %in% names(R))){
        stop(paste0(i$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[i$x]])){
        stop(paste0(i$x, " is not numeric."))
      }

      if (!is.null(i$na.rm) && i$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_iqr <- c(code_iqr, paste0(
        #i$name, " = IQR(", i$x, na_1
        i$name, " = tryCatch(IQR(", i$x, na_1, " error = function(e){return(NA)}),"
      ))

    }
  } else {
    code_iqr <- ""
  }


  ### Parse quantile summary code
  if (!is.null(o_args$quantile)) {

    code_q <- ""
    for (q in o_args$quantile) {

      # handle missing x, or prob argument
      if (is.null(q$x)){
        stop("`x` argument is required.")
      }
      if (is.null(q$prob)){
        stop("`prob` argument is required.")
      }

      # if name missing, create a name
      if (is.null(q$name)){
        q$name <- paste0("quantile_", q$prob, "_", q$x)
      }

      if (!is.character(q$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(q$x %in% names(R))){
        stop(paste0(q$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[q$x]])){
        stop(paste0(q$x, " is not numeric."))
      }
      # handle prob that isn't a number
      if (!is.numeric(q$prob)){
        stop(paste0(q$prob, " is not numeric."))
      }
      # handle prob that isn't between 0 and 1
      if (length(q$prob) > 1 | q$prob > 1 | q$prob < 0){
        stop(paste0(q$prob, " is not a number between 0 and 1."))
      }

      if (!is.null(q$na.rm) && q$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_q <- c(code_q, paste0(
        #q$name, " = quantile(", q$x, ", probs=", q$prob, ",", na_1
        q$name, " = tryCatch(quantile(", q$x, ", probs=", q$prob, ",",
                na_1, " error = function(e){return(NA)}),"
      ))

    }
  } else {
    code_q <- ""
  }


  ### Parse min summary code
  if (!is.null(o_args$min)) {

    code_min <- ""
    for (m in o_args$min) {

      # handle missing x argument
      if (is.null(m$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("min_", m$x)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(m$x %in% names(R))){
        stop(paste0(m$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[m$x]])){
        stop(paste0(m$x, " is not numeric."))
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_min <- c(code_min, paste0(
        m$name, " = min(", m$x, na_1
      ))

    }
  } else {
    code_min <- ""
  }

  ### Parse max summary code
  if (!is.null(o_args$max)) {

    code_max <- ""
    for (m in o_args$max) {

      # handle missing x argument
      if (is.null(m$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("max_", m$x)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(m$x %in% names(R))){
        stop(paste0(m$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[m$x]])){
        stop(paste0(m$x, " is not numeric."))
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_max <- c(code_max, paste0(
        m$name, " = max(", m$x, na_1
      ))

    }
  } else {
    code_max <- ""
  }


  ### Parse median summary code
  if (!is.null(o_args$median)) {

    code_median<- ""
    for (m in o_args$median) {

      # handle missing x argument
      if (is.null(m$x)){
        stop("`x` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("median_", m$x)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle x that does not refer to columns in results
      if (!(m$x %in% colnames(R))){
        stop(paste0(m$x, " is not a variable in results."))
      }
      # handle non-numeric x
      if (!is.numeric(R[[m$x]])){
        stop(paste0(m$x, " is not numeric."))
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_median <- c(code_median, paste0(
        m$name, " = median(", m$x, na_1
      ))

    }
  } else {
    code_median <- ""
  }


  ### Calculate bias and parse summary code
  if (!is.null(o_args$bias)) {

    code_bias <- ""
    for (b in o_args$bias) {

      # handle missing estimate or truth argument
      if (is.null(b$estimate)){
        stop("`estimate` argument is required.")
      }
      if (is.null(b$truth)){
        stop("`truth` argument is required.")
      }

      # if name missing, create a name
      if (is.null(b$name)){
        b$name <- paste0("bias_", b$estimate)
      }

      if (!is.character(b$name)){
        stop("`name` must be a character string.")
      }

      # handle truth and/or estimate that do not refer to columns in results
      if (!(b$estimate %in% names(R))){
        stop(paste0(b$estimate, " is not a variable in results."))
      }
      if (is.character(b$truth) & !(b$truth %in% names(R))){
        stop(paste0(b$truth, " is not a variable in results."))
      }
      # handle non-numeric truth and/or estimate
      if (!is.numeric(R[[b$estimate]])){
        stop(paste0(b$estimate, " is not numeric."))
      }
      if (is.character(b$truth)){
        if (!is.numeric(R[[b$truth]])){
          stop(paste0(b$truth, " is not numeric."))
        }
      }
      # !!!!! unsure if we should allow vectors of numbers for `truth`
      else{
        if (!is.numeric(b$truth) | length(b$truth) > 1){
          stop(paste0(b$truth, " is neither a number nor a variable in results."))
        }
      }


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


  ### Calculate bias and parse summary code
  if (!is.null(o_args$bias_pct)) {

    code_bias_pct <- ""
    for (b in o_args$bias_pct) {

      # handle missing estimate or truth argument
      if (is.null(b$estimate)){
        stop("`estimate` argument is required.")
      }
      if (is.null(b$truth)){
        stop("`truth` argument is required.")
      }

      # if name missing, create a name
      if (is.null(b$name)){
        b$name <- paste0("bias_pct_", b$estimate)
      }

      if (!is.character(b$name)){
        stop("`name` must be a character string.")
      }

      # handle truth and/or estimate that do not refer to columns in results
      if (!(b$estimate %in% names(R))){
        stop(paste0(b$estimate, " is not a variable in results."))
      }
      if (is.character(b$truth) & !(b$truth %in% names(R))){
        stop(paste0(b$truth, " is not a variable in results."))
      }
      # handle non-numeric truth and/or estimate
      if (!is.numeric(R[[b$estimate]])){
        stop(paste0(b$estimate, " is not numeric."))
      }
      if (is.character(b$truth)){
        if (!is.numeric(R[[b$truth]])){
          stop(paste0(b$truth, " is not numeric."))
        }
      }
      # !!!!! unsure if we should allow vectors of numbers for `truth`
      else{
        if (!is.numeric(b$truth) | length(b$truth) > 1){
          stop(paste0(b$truth, " is neither a number nor a variable in results."))
        }
      }


      if (!is.null(b$na.rm) && b$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
      } else {
        na_1 <- ")"
      }

      code_bias_pct <- c(code_bias_pct, paste0(
        b$name, " = mean(", b$estimate, "-", b$truth, na_1, "/abs(", b$truth, "),"
      ))

    }

  } else {
    code_bias_pct <- ""
  }


  ### Calculate MSE and parse summary code
  if (!is.null(o_args$mse)) {

    code_mse <- ""
    for (m in o_args$mse) {

      # handle missing estimate or truth argument
      if (is.null(m$estimate)){
        stop("`estimate` argument is required.")
      }
      if (is.null(m$truth)){
        stop("`truth` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MSE_", m$estimate)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle truth and/or estimate that do not refer to columns in results
      if (!(m$estimate %in% names(R))){
        stop(paste0(m$estimate, " is not a variable in results."))
      }
      if (is.character(m$truth) & !(m$truth %in% names(R))){
        stop(paste0(m$truth, " is not a variable in results."))
      }
      # handle non-numeric truth and/or estimate
      if (!is.numeric(R[[m$estimate]])){
        stop(paste0(m$estimate, " is not numeric."))
      }
      if (is.character(m$truth)){
        if (!is.numeric(R[[m$truth]])){
          stop(paste0(m$truth, " is not numeric."))
        }
      }
      else{
        if (!is.numeric(m$truth) | length(m$truth) > 1){
          stop(paste0(m$truth, " is neither a number nor a variable in results."))
        }
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mse <- c(code_mse, paste0(
        m$name, " = mean((", m$estimate, "-", m$truth, ")^2", na_1
      ))

    }

  } else {
    code_mse <- ""
  }


  ### Calculate MAE and parse summary code
  if(!is.null(o_args$mae)) {

    code_mae <- ""
    for (m in o_args$mae) {

      # handle missing estimate or truth argument
      if (is.null(m$estimate)){
        stop("`estimate` argument is required.")
      }
      if (is.null(m$truth)){
        stop("`truth` argument is required.")
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MAE_", m$estimate)
      }

      if (!is.character(m$name)){
        stop("`name` must be a character string.")
      }

      # handle truth and/or estimate that do not refer to columns in results
      if (!(m$estimate %in% names(R))){
        stop(paste0(m$estimate, " is not a variable in results."))
      }
      if (is.character(m$truth) & !(m$truth %in% names(R))){
        stop(paste0(m$truth, " is not a variable in results."))
      }
      # handle non-numeric truth and/or estimate
      if (!is.numeric(R[[m$estimate]])){
        stop(paste0(m$estimate, " is not numeric."))
      }
      if (is.character(m$truth)){
        if (!is.numeric(R[[m$truth]])){
          stop(paste0(m$truth, " is not numeric."))
        }
      }
      else{
        if (!is.numeric(m$truth) | length(m$truth) > 1){
          stop(paste0(m$truth, " is neither a number nor a variable in results.."))
        }
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mae <- c(code_mae, paste0(
        m$name, " = mean(abs(", m$estimate, "-", m$truth, ")", na_1
      ))

    }

  } else {
    code_mae <- ""
  }


  ### Calculate CIs and parse coverage summary code
  # !!!!! Add a column to specify how many rows were omitted with na.rm (for other summary stats as well)
  # !!!!! if (mean, se) and (upper, lower) are both provided, the latter takes precedence.
  if (!is.null(o_args$coverage)) {

    code_cov <- ""
    for (cov in o_args$coverage) {

      # make sure user supplies either est + se, or upper + lower, as well as name
      if (is.null(cov$name)){
        stop("`name` argument is required.")
      }
      if (is.null(cov$truth)){
        stop("`truth` argument is required.")
      }
      if (is.character(cov$truth) & !(cov$truth %in% names(R))){
        stop(paste0(cov$truth, " is not a variable in results."))
      }
      if (is.character(cov$truth)){
        if (!is.numeric(R[[cov$truth]])){
          stop(paste0(cov$truth, " is not numeric."))
        }
      }
      else{
        if (!is.numeric(cov$truth) | length(cov$truth) > 1){
          stop(paste0(cov$truth, " is neither a number nor a variable in results.."))
        }
      }
      if (is.null(cov$lower) & is.null(cov$upper) & is.null(cov$estimate) & is.null(cov$se)){
        stop("Either `estimate` and `se` OR `lower` and `upper` must be provided.")
      }
      if (is.null(cov$se) & is.null(cov$estimate) & xor(is.null(cov$upper), is.null(cov$lower))){
        stop("Both `lower` and `upper` must be provided.")
      }
      if (is.null(cov$lower) & is.null(cov$upper) & xor(is.null(cov$estimate), is.null(cov$se))){
        stop("Both `estimate` and `se` must be provided.")
      }

      # make sure everything is the proper class
      if (!is.character(cov$name)){
        stop("`name` must be a character string.")
      }

      if (!is.null(cov$se) & !is.null(cov$estimate)) {
        if (!(cov$estimate %in% names(R))){
          stop(paste0(cov$estimate, " is not a variable in results."))
        }
        # handle non-numeric truth and/or estimate
        if (!is.numeric(R[[cov$estimate]])){
          stop(paste0(cov$estimate, " is not numeric."))
        }
        if (!(cov$se %in% names(R))){
          stop(paste0(cov$se, " is not a variable in results."))
        }
        # handle non-numeric truth and/or estimate
        if (!is.numeric(R[[cov$se]])){
          stop(paste0(cov$se, " is not numeric."))
        }
        ci_l <- R[[cov$estimate]] - 1.96*R[[cov$se]]
        ci_h <- R[[cov$estimate]] + 1.96*R[[cov$se]]
      }

      if (!is.null(cov$lower) && !is.null(cov$upper)) {
        if (!(cov$lower %in% names(R))){
          stop(paste0(cov$lower, " is not a variable in results."))
        }
        # handle non-numeric truth and/or estimate
        if (!is.numeric(R[[cov$lower]])){
          stop(paste0(cov$lower, " is not numeric."))
        }
        if (!(cov$upper %in% names(R))){
          stop(paste0(cov$upper, " is not a variable in results."))
        }
        # handle non-numeric truth and/or estimate
        if (!is.numeric(R[[cov$upper]])){
          stop(paste0(cov$upper, " is not numeric."))
        }
        ci_l <- R[[cov$lower]]
        ci_h <- R[[cov$upper]]
      }

      R[[paste0(".ci_l_",cov$name)]] <- ci_l
      R[[paste0(".ci_h_",cov$name)]] <- ci_h

      if (!is.null(cov$na.rm) && cov$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
        #na_2 <- paste0(cov$name, "_num_na", " = sum(is.na(", cov$estimate,
         #              ") | is.na(", cov$se,")),")
      } else {
        na_1 <- ")"
        #na_2 <- ""
      }

      code_cov <- c(code_cov, paste0(
        cov$name, " = sum(.ci_l_", cov$name, " <= ", cov$truth,
        " & ", cov$truth, " <= .ci_h_", cov$name, na_1,
        "/sum(!is.na(.ci_l_", cov$name, ") & !is.na(.ci_h_", cov$name,
        ") & !is.na(", cov$truth, ")", "),"))#, na_2))
    }

  } else {
    code_cov <- ""
  }


  ### Put code strings together
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(dplyr::group_by(R, level_id),",
    code_levels,
    code_mean,
    code_median,
    code_var,
    code_sd,
    code_mad,
    code_iqr,
    code_min,
    code_max,
    code_q,
    code_bias,
    code_bias_pct,
    code_mse,
    code_mae,
    code_cov)
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))

  return (summary)

}
