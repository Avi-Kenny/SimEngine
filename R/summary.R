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

  # String to temporarily append to column names
  pre <- "o___o_"

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
  if (!is.null(o_args[["mean"]])) {

    code_mean <- ""
    for (m in o_args$mean) {

      # Handle errors
      handle_errors(m$x, "is.null", msg="`x` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$x, "is.in", other=names(R),
                    msg=paste0("`",m$x,"` is not a variable in results."))
      handle_errors(R[[m$x]], "is.numeric.vec", name=m$x)

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("mean_", m$x)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mean <- c(code_mean, paste0(
        pre, m$name, " = mean(", m$x, na_1
      ))

    }
  } else {
    code_mean <- ""
  }

  ### Parse SD summary code
  if (!is.null(o_args[["sd"]])) {

    code_sd <- ""
    for (sd in o_args$sd) {

      # Handle errors
      handle_errors(sd$x, "is.null", msg="`x` argument is required.")
      handle_errors(sd$name, "is.character", name="name")
      handle_errors(sd$x, "is.in", other=names(R),
                    msg=paste0("`",sd$x,"` is not a variable in results."))
      handle_errors(R[[sd$x]], "is.numeric.vec", name=sd$x)

      # if name missing, create a name
      if (is.null(sd$name)){
        sd$name <- paste0("sd_", sd$x)
      }

      if (!is.null(sd$na.rm) && sd$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_sd <- c(code_sd, paste0(
        pre, sd$name, " = sd(", sd$x, na_1
      ))

    }
  } else {
    code_sd <- ""
  }

  ### Parse variance summary code
  if (!is.null(o_args[["var"]])) {

    code_var <- ""
    for (var in o_args$var) {

      # Handle errors
      handle_errors(var$x, "is.null", msg="`x` argument is required.")
      handle_errors(var$name, "is.character", name="name")
      handle_errors(var$x, "is.in", other=names(R),
                    msg=paste0("`",var$x,"` is not a variable in results."))
      handle_errors(R[[var$x]], "is.numeric.vec", name=var$x)

      # if name missing, create a name
      if (is.null(var$name)){
        var$name <- paste0("var_", var$x)
      }

      if (!is.null(var$na.rm) && var$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_var <- c(code_var, paste0(
        pre, var$name, " = var(", var$x, na_1
      ))

    }
  } else {
    code_var <- ""
  }


  ### Parse MAD summary code
  if (!is.null(o_args[["mad"]])) {

    code_mad <- ""
    for (m in o_args$mad) {

      # Handle errors
      handle_errors(m$x, "is.null", msg="`x` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$x, "is.in", other=names(R),
                    msg=paste0("`",m$x,"` is not a variable in results."))
      handle_errors(R[[m$x]], "is.numeric.vec", name=m$x)

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MAD_", m$x)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mad <- c(code_mad, paste0(
        pre, m$name, " = mad(", m$x, na_1
      ))

    }
  } else {
    code_mad <- ""
  }


  ### Parse IQR summary code
  if (!is.null(o_args[["iqr"]])) {

    code_iqr <- ""
    for (i in o_args$iqr) {

      # Handle errors
      handle_errors(i$x, "is.null", msg="`x` argument is required.")
      handle_errors(i$name, "is.character", name="name")
      handle_errors(i$x, "is.in", other=names(R),
                    msg=paste0("`",i$x,"` is not a variable in results."))
      handle_errors(R[[i$x]], "is.numeric.vec", name=i$x)

      # if name missing, create a name
      if (is.null(i$name)){
        i$name <- paste0("IQR_", i$x)
      }

      if (!is.null(i$na.rm) && i$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_iqr <- c(code_iqr, paste0(
        #i$name, " = IQR(", i$x, na_1
        pre, i$name, " = tryCatch(IQR(", i$x, na_1,
        " error = function(e){return(NA)}),"
      ))

    }
  } else {
    code_iqr <- ""
  }


  ### Parse quantile summary code
  if (!is.null(o_args[["quantile"]])) {

    code_q <- ""
    for (q in o_args$quantile) {

      # Handle errors
      handle_errors(q$x, "is.null", msg="`x` argument is required.")
      handle_errors(q$prob, "is.null", msg="`prob` argument is required.")
      handle_errors(q$name, "is.character", name="name")
      handle_errors(q$x, "is.in", other=names(R),
                    msg=paste0("`",q$x,"` is not a variable in results."))
      handle_errors(R[[q$x]], "is.numeric.vec", name=q$x)
      handle_errors(q$prob, "is.numeric", name=q$x)
      if (length(q$prob) > 1 | q$prob > 1 | q$prob < 0){
        stop(paste0(q$prob, " is not a number between 0 and 1."))
      }

      # if name missing, create a name
      if (is.null(q$name)){
        q$name <- paste0("quantile_", q$prob, "_", q$x)
      }

      if (!is.null(q$na.rm) && q$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_q <- c(code_q, paste0(
        #q$name, " = quantile(", q$x, ", probs=", q$prob, ",", na_1
        pre, q$name, " = tryCatch(quantile(", q$x, ", probs=", q$prob, ",",
        na_1, " error = function(e){return(NA)}),"
      ))

    }
  } else {
    code_q <- ""
  }


  ### Parse min summary code
  if (!is.null(o_args[["min"]])) {

    code_min <- ""
    for (m in o_args$min) {

      # Handle errors
      handle_errors(m$x, "is.null", msg="`x` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$x, "is.in", other=names(R),
                    msg=paste0("`",m$x,"` is not a variable in results."))
      handle_errors(R[[m$x]], "is.numeric.vec", name=m$x)

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("min_", m$x)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_min <- c(code_min, paste0(
        pre, m$name, " = min(", m$x, na_1
      ))

    }
  } else {
    code_min <- ""
  }

  ### Parse max summary code
  if (!is.null(o_args[["max"]])) {

    code_max <- ""
    for (m in o_args$max) {

      # Handle errors
      handle_errors(m$x, "is.null", msg="`x` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$x, "is.in", other=names(R),
                    msg=paste0("`",m$x,"` is not a variable in results."))
      handle_errors(R[[m$x]], "is.numeric.vec", name=m$x)

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("max_", m$x)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_max <- c(code_max, paste0(
        pre, m$name, " = max(", m$x, na_1
      ))

    }
  } else {
    code_max <- ""
  }


  ### Parse median summary code
  if (!is.null(o_args[["median"]])) {

    code_median<- ""
    for (m in o_args$median) {

      # Handle errors
      handle_errors(m$x, "is.null", msg="`x` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$x, "is.in", other=names(R),
                    msg=paste0("`",m$x,"` is not a variable in results."))
      handle_errors(R[[m$x]], "is.numeric.vec", name=m$x)

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("median_", m$x)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_median <- c(code_median, paste0(
        pre, m$name, " = median(", m$x, na_1
      ))

    }
  } else {
    code_median <- ""
  }


  ### Calculate bias and parse summary code
  if (!is.null(o_args[["bias"]])) {

    code_bias <- ""
    for (b in o_args$bias) {

      # Handle errors
      handle_errors(b$estimate, "is.null", msg="`estimate` argument is required.")
      handle_errors(b$truth, "is.null", msg="`truth` argument is required.")
      handle_errors(b$name, "is.character", name="name")
      handle_errors(b$estimate, "is.in", other=names(R),
                    msg=paste0("`",b$estimate,"` is not a variable in results."))
      handle_errors(R[[b$estimate]], "is.numeric.vec", name=b$estimate)
      if (length(b$truth)>1 || (
        !is.numeric(b$truth) && !(b$truth %in% names(R))
      )) {
        stop(paste0(b$truth, " is neither a number nor a variable in results."))
      }

      # if name missing, create a name
      if (is.null(b$name)){
        b$name <- paste0("bias_", b$estimate)
      }

      if (!is.null(b$na.rm) && b$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_bias <- c(code_bias, paste0(
        pre, b$name, " = mean(", b$estimate, "-", b$truth, na_1
      ))

    }

  } else {
    code_bias <- ""
  }


  ### Calculate percent bias and parse summary code
  if (!is.null(o_args[["bias_pct"]])) {

    code_bias_pct <- ""
    for (b in o_args$bias_pct) {

      # Handle errors
      handle_errors(b$estimate, "is.null",msg="`estimate` argument is required.")
      handle_errors(b$truth, "is.null", msg="`truth` argument is required.")
      handle_errors(b$name, "is.character", name="name")
      handle_errors(b$estimate, "is.in", other=names(R),
                    msg=paste0("`",b$estimate,"` is not a variable in results."))
      handle_errors(R[[b$estimate]], "is.numeric.vec", name=b$estimate)
      if (length(b$truth)>1 || (
        !is.numeric(b$truth) && !(b$truth %in% names(R))
      )) {
        stop(paste0(b$truth, " is neither a number nor a variable in results."))
      }

      # if name missing, create a name
      if (is.null(b$name)){
        b$name <- paste0("bias_pct_", b$estimate)
      }

      if (!is.null(b$na.rm) && b$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
      } else {
        na_1 <- ")"
      }

      code_bias_pct <- c(code_bias_pct, paste0(
        pre, b$name, " = mean(", b$estimate, "-", b$truth, na_1,
        "/abs(", b$truth, "[1]),"
      ))

    }

  } else {
    code_bias_pct <- ""
  }


  ### Calculate MSE and parse summary code
  if (!is.null(o_args[["mse"]])) {

    code_mse <- ""
    for (m in o_args$mse) {

      # Handle errors
      handle_errors(m$estimate, "is.null", msg="`estimate` argument is required.")
      handle_errors(m$truth, "is.null", msg="`truth` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$estimate, "is.in", other=names(R),
                    msg=paste0("`",m$estimate,"` is not a variable in results."))
      handle_errors(R[[m$estimate]], "is.numeric.vec", name=m$estimate)
      if (length(m$truth)>1 || (
        !is.numeric(m$truth) && !(m$truth %in% names(R))
      )) {
        stop(paste0(m$truth, " is neither a number nor a variable in results."))
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MSE_", m$estimate)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mse <- c(code_mse, paste0(
        pre, m$name, " = mean((", m$estimate, "-", m$truth, ")^2", na_1
      ))

    }

  } else {
    code_mse <- ""
  }


  ### Calculate MAE and parse summary code
  if(!is.null(o_args[["mae"]])) {

    code_mae <- ""
    for (m in o_args$mae) {

      # Handle errors
      handle_errors(m$estimate, "is.null", msg="`estimate` argument is required.")
      handle_errors(m$truth, "is.null", msg="`truth` argument is required.")
      handle_errors(m$name, "is.character", name="name")
      handle_errors(m$estimate, "is.in", other=names(R),
                    msg=paste0("`",m$estimate,"` is not a variable in results."))
      handle_errors(R[[m$estimate]], "is.numeric.vec", name=m$estimate)
      if (length(m$truth)>1 || (
        !is.numeric(m$truth) && !(m$truth %in% names(R))
      )) {
        stop(paste0(m$truth, " is neither a number nor a variable in results."))
      }

      # if name missing, create a name
      if (is.null(m$name)){
        m$name <- paste0("MAE_", m$estimate)
      }

      if (!is.null(m$na.rm) && m$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mae <- c(code_mae, paste0(
        pre, m$name, " = mean(abs(", m$estimate, "-", m$truth, ")", na_1
      ))

    }

  } else {
    code_mae <- ""
  }


  ### Calculate CIs and parse coverage summary code
  # !!!!! Add a column to specify how many rows were omitted with na.rm (for other summary stats as well)
  # !!!!! if (mean, se) and (upper, lower) are both provided, the latter takes precedence.
  if (!is.null(o_args[["coverage"]])) {

    code_cov <- ""
    for (cov in o_args$coverage) {

      # Handle errors
      handle_errors(cov$name, "is.null", msg="`name` argument is required.")
      handle_errors(cov$truth, "is.null", msg="`truth` argument is required.")
      handle_errors(cov$name, "is.character", name="name")
      if (length(cov$truth)>1 || (
        !is.numeric(cov$truth) && !(cov$truth %in% names(R))
      )) {
        stop(paste0(cov$truth, " is neither a number nor a variable in results."))
      }
      if (!((!is.null(cov$est) && !is.null(cov$se)) ||
            (!is.null(cov$lower) && !is.null(cov$upper)))) {
        stop("Either `estimate` and `se` OR `lower` and `upper` must be provided.")
      }

      # Handle case where user provides estimate+se
      if (!is.null(cov$se) && !is.null(cov$estimate)) {
        handle_errors(cov$estimate, "is.in", other=names(R),
                      msg=paste0("`",cov$estimate,"` is not a variable in results."))
        handle_errors(cov$se, "is.in", other=names(R),
                      msg=paste0("`",cov$se,"` is not a variable in results."))
        handle_errors(R[[cov$estimate]], "is.numeric.vec", name=cov$estimate)
        handle_errors(R[[cov$se]], "is.numeric.vec", name=cov$se)
        ci_l <- R[[cov$estimate]] - 1.96*R[[cov$se]]
        ci_h <- R[[cov$estimate]] + 1.96*R[[cov$se]]
      }

      # Handle case where user provides lower+upper
      if (!is.null(cov$lower) && !is.null(cov$upper)) {
        handle_errors(cov$lower, "is.in", other=names(R),
                      msg=paste0("`",cov$lower,"` is not a variable in results."))
        handle_errors(cov$upper, "is.in", other=names(R),
                      msg=paste0("`",cov$upper,"` is not a variable in results."))
        handle_errors(R[[cov$lower]], "is.numeric.vec", name=cov$lower)
        handle_errors(R[[cov$upper]], "is.numeric.vec", name=cov$upper)
        ci_l <- R[[cov$lower]]
        ci_h <- R[[cov$upper]]
      }

      R[[paste0(".ci_l_",cov$name)]] <- ci_l
      R[[paste0(".ci_h_",cov$name)]] <- ci_h

      if (!is.null(cov$na.rm) && cov$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
      } else {
        na_1 <- ")"
      }

      code_cov <- c(code_cov, paste0(
        pre, cov$name, " = sum(.ci_l_", cov$name, " <= ", cov$truth,
        " & ", cov$truth, " <= .ci_h_", cov$name, na_1,
        "/sum(!is.na(.ci_l_", cov$name, ") & !is.na(.ci_h_", cov$name,
        ") & !is.na(", cov$truth, ")", "),"))
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
  names(summary) <- gsub(pre, "", names(summary))

  return (summary)

}
