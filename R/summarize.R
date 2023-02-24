#' Summarize simulation results
#'
#' @description This function calculates summary statistics for simulation results.
#'     Options for summary statistics include descriptive statistics (e.g. measures of
#'     center or spread) and inferential statistics (e.g. bias or confidence interval
#'     coverage). All summary statistics are calculated over simulation replicates
#'     within a single simulation level.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param ... Name-value pairs of summary statistic functions. The possible
#'     functions (names) are listed below. The value for each summary
#'     function is a list of summaries to perform.
#'     \itemize{
#'     \item{\code{mean}: Each \code{mean} summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the mean, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{median}: Each \code{median} summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the median, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{var}: Each \code{var} (variance) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the variance, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{sd}: Each \code{sd} (standard deviation) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the standard deviation, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{mad}: Each \code{mad} (mean absolute deviation) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the MAD, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{iqr}: Each \code{iqr} (interquartile range) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the IQR, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{min}: Each \code{min} (minimum) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the minimum, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{max}: Each \code{max} (maximum) summary is a named list of three arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the maximum, and \code{na.rm} indicates whether to exclude \code{NA}
#'     values when performing the calculation.}
#'
#'     \item{\code{quantile}: Each \code{quantile} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{x} gives the name of the variable in \code{sim$results}
#'     on which to calculate the quantile, \code{prob} is a number in [0,1] denoting the desired quantile,
#'     and \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{bias}: Each \code{bias} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{bias_pct}: Each \code{bias_pct} summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{mse}: Each \code{mse} (mean squared error) summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{mae}: Each \code{mae} (mean absolute error) summary is a named list of four arguments. \code{name} gives
#'     a name for the summary, \code{estimate} gives the name of the variable in \code{sim$results}
#'     containing the estimator of interest, \code{truth} is the estimand of interest (see \emph{Details}), and
#'     \code{na.rm} indicates whether to exclude \code{NA} values when performing the calculation.}
#'
#'     \item{\code{coverage}: Each \code{coverage} (confidence interval coverage) summary is a named list of five arguments. Either
#'     (\code{estimate}, \code{se}) or (\code{lower}, \code{upper}) must be provided.  \code{name} gives a name for the
#'     summary, \code{estimate} gives the name of the variable in \code{sim$results}
#'     containing the estimator of interest, \code{se} gives the name of the variable in \code{sim$results}
#'     containing the standard error of the estimator of interest, \code{lower} gives the name of the variable in
#'     \code{sim$results} containing the confidence interval lower bound, \code{upper} gives the name of the
#'     variable in \code{sim$results} containing the confidence interval upper bound, \code{truth} is the
#'     estimand of interest, and \code{na.rm} indicates whether to exclude \code{NA} values when performing the
#'     calculation. See \emph{Details}.}
#'    }
#' @details \itemize{
#'     \item{For all summaries besides \code{coverage}, the \code{name} argument is optional. If \code{name} is not provided,
#'     a name will be formed from the type of summary and the column on which the summary is performed.}
#'
#'     \item{For all inferential summaries there are three ways to specify \code{truth}: (1) a single number,
#'     meaning the estimand is the same across all simulation replicates and levels, (2) a numeric vector of the
#'     same length as the number of rows in \code{sim$results}, or (3) the name of a variable in \code{sim$results}
#'     containing the estimand of interest.}
#'
#'     \item{There are two ways to specify the confidence interval bounds for \code{coverage}. The first is to provide
#'     an \code{estimate} and its associated \code{se} (standard error). These should both be variables in
#'     \code{sim$results}. The function constructs a 95\% Wald-type confidence interval of the form
#'     (\code{estimate} - 1.96 \code{se}, \code{estimate} + 1.96 \code{se}).  The alternative is to provide
#'     \code{lower} and \code{upper} bounds, which should also be variables in \code{sim$results}. In this case,
#'     the confidence interval is (\code{lower}, \code{upper}). The coverage is simply the proportion of simulation
#'     replicates for a given level in which \code{truth} lies within the interval.}
#' }
#' @return A data frame containing the result of each specified summary function as a column, for each of
#'     the simulation levels.
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the summarize function.
#' sim <- new_sim()
#' create_data <- function(n) { rpois(n, lambda=5) }
#' est_mean <- function(dat, type) {
#'   if (type=="M") { return(mean(dat)) }
#'   if (type=="V") { return(var(dat)) }
#' }
#' sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
#' sim %<>% set_config(num_sim=5)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- est_mean(dat=dat, type=L$est)
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim %>% summarize(
#'   mean = list(name="mean_lambda_hat", x="lambda_hat"),
#'   mse = list(name="lambda_mse", estimate="lambda_hat", truth=5)
#' )
#' @export
summarize <- function(sim, ...) {
  UseMethod("summarize")
}

#' @export
summarize.sim_obj <- function(sim, ...) {

  # Error handling
  if (sim$vars$run_state == "pre run") {
    stop("Simulation has not been run yet.")
  }
  if (sim$vars$run_state == "run, all errors") {
    stop("100% of simulations had errors.")
  }

  # Parse passed arguments
  R <- sim$results
  names_levels <- names(sim$levels)
  names_results <- names(sim$results)

  # Evaluate passed arguments, passing in constants
  o_args <- list(...)

  # String to temporarily append to column names
  pre <- "o___o_"

  # If no additional arguments provided to summarize, display means by default
  # fix this to go with new structure
  # if (identical(o_args,list())) {
  #
  #   names_means <- names_results[!(names_results %in% c(
  #     names_levels, "sim_uid", "rep_id", "level_id"
  #   ))]
  #
  #   o_args <- list()
  #   for (i in 1:length(names_means)) {
  #     o_args[[i]] <- list(name=paste0("mean_",names_means[i]), x=names_means[i])
  #   }
  #   o_args <- list(mean=o_args)
  #
  # }

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
               "coverage",
               "correlation",
               "covariance")

  code_mean <- ""
  code_median <- ""
  code_var <- ""
  code_sd <- ""
  code_mad <- ""
  code_iqr <- ""
  code_quantile <- ""
  code_min <- ""
  code_max <- ""
  code_bias <- ""
  code_bias_pct <- ""
  code_mse <- ""
  code_mae <- ""
  code_coverage <- ""
  code_correlation <- ""
  code_covariance <- ""
  
  
  # Parse code to display levels
  if (is.null(sim$levels$no_levels)) {
    code_levels <- paste0("'",names_levels,"'=`",names_levels,"`[1],")
  } else {
    code_levels <- ""
  }

  for (arg in o_args){

    if (!(methods::is(arg, "list"))){
      stop(paste0("Each desired summary metric must be specified as a list."))
    }


    # get stat name provided by user, make sure it's valid (and it exists)
    stat_name <- arg$stat
    if (is.null(stat_name)){
      stop(paste0("You must provide a type of summary metric."))
    }
    if (!(stat_name %in% metrics)){
      stop(paste0(stat_name, " is an invalid summary metric."))
    }

    # parse mean code
    if (stat_name == "mean"){
      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("mean_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mean <- c(code_mean, paste0(
        pre, arg$name, " = mean(", arg$x, na_1
      ))

      # parse median code
    } else if (stat_name == "median"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("median_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_median <- c(code_median, paste0(
        pre, arg$name, " = median(", arg$x, na_1
      ))

      # parse variance code
    } else if (stat_name == "var"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("var_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_var <- c(code_var, paste0(
        pre, arg$name, " = var(", arg$x, na_1
      ))

      # parse SD code
    } else if (stat_name == "sd"){
      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("sd_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_sd <- c(code_sd, paste0(
        pre, arg$name, " = sd(", arg$x, na_1
      ))

      # parse MAD code
    } else if (stat_name == "mad"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("MAD_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mad <- c(code_mad, paste0(
        pre, arg$name, " = mad(", arg$x, na_1
      ))


      # parse IQR code
    } else if (stat_name == "iqr"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("IQR_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_iqr <- c(code_iqr, paste0(
        #arg$name, " = IQR(", arg$x, na_1
        pre, arg$name, " = tryCatch(IQR(", arg$x, na_1,
        " error = function(e) {return(NA)}),"
      ))

      # parse quantile code
    } else if (stat_name == "quantile"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("quantile_", arg$prob, "_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$prob, "is.null", msg="`prob` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)
      handle_errors(arg$prob, "is.numeric", name=arg$prob)
      if (length(arg$prob) > 1 | arg$prob > 1 | arg$prob < 0) {
        stop(paste0(arg$prob, " is not a number between 0 and 1."))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_quantile <- c(code_quantile, paste0(
        #arg$name, " = quantile(", arg$x, ", probs=", arg$prob, ",", na_1
        pre, arg$name, " = tryCatch(quantile(", arg$x, ", probs=", arg$prob, ",",
        na_1, " error = function(e) {return(NA)}),"
      ))

      # parse min summary code
    } else if (stat_name == "min"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("min_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_min <- c(code_min, paste0(
        pre, arg$name, " = min(", arg$x, na_1
      ))

      # parse max summary code
    } else if (stat_name == "max"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("max_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_max <- c(code_max, paste0(
        pre, arg$name, " = max(", arg$x, na_1
      ))

      # parse bias code
    } else if (stat_name == "bias"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("bias_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null", msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,"` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth, "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_bias <- c(code_bias, paste0(
        pre, arg$name, " = mean(", arg$estimate, "-", arg$truth, na_1
      ))


      # parse bias pct code
    } else if (stat_name == "bias_pct"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("bias_pct_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null",msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,"` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth, "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
      } else {
        na_1 <- ")"
      }

      code_bias_pct <- c(code_bias_pct, paste0(
        pre, arg$name, " = mean(", arg$estimate, "-", arg$truth, na_1,
        "/abs(", arg$truth, "[1]),"
      ))

      # parse MSE code
    } else if (stat_name == "mse"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("MSE_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null", msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,"` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth, "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mse <- c(code_mse, paste0(
        pre, arg$name, " = mean((", arg$estimate, "-", arg$truth, ")^2", na_1
      ))

      # parse MAE code
    } else if (stat_name == "mae"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("MAE_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null", msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,"` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth, "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mae <- c(code_mae, paste0(
        pre, arg$name, " = mean(abs(", arg$estimate, "-", arg$truth, ")", na_1
      ))

      ### Calculate CIs and parse coverage summary code
      # !!!!! Add a column to specify how many rows were omitted with na.rm (for other summary stats as well)
      # !!!!! if (mean, se) and (upper, lower) are both provided, the latter takes precedence.
    } else if (stat_name == "coverage"){
      # Handle errors
      handle_errors(arg$name, "is.null", msg="`name` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth, "` is neither a number nor a variable in results"))
      }
      if (!((!is.null(arg$est) && !is.null(arg$se)) ||
            (!is.null(arg$lower) && !is.null(arg$upper)))) {
        stop("Either `estimate` and `se` OR `lower` and `upper` must be provided")
      }

      # Handle case where user provides estimate+se
      if (!is.null(arg$se) && !is.null(arg$estimate)) {
        handle_errors(arg$estimate, "is.in", other=names(R),
                      msg=paste0("`",arg$estimate,"` is not a variable in results"))
        handle_errors(arg$se, "is.in", other=names(R),
                      msg=paste0("`",arg$se,"` is not a variable in results"))
        handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
        handle_errors(R[[arg$se]], "is.numeric.vec", name=arg$se)
        ci_l <- R[[arg$estimate]] - 1.96*R[[arg$se]]
        ci_h <- R[[arg$estimate]] + 1.96*R[[arg$se]]
      }

      # Handle case where user provides lower+upper
      if (!is.null(arg$lower) && !is.null(arg$upper)) {
        handle_errors(arg$lower, "is.in", other=names(R),
                      msg=paste0("`",arg$lower,"` is not a variable in results"))
        handle_errors(arg$upper, "is.in", other=names(R),
                      msg=paste0("`",arg$upper,"` is not a variable in results"))
        handle_errors(R[[arg$lower]], "is.numeric.vec", name=arg$lower)
        handle_errors(R[[arg$upper]], "is.numeric.vec", name=arg$upper)
        ci_l <- R[[arg$lower]]
        ci_h <- R[[arg$upper]]
      }

      R[[paste0(".ci_l_",arg$name)]] <- ci_l
      R[[paste0(".ci_h_",arg$name)]] <- ci_h

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE)"
      } else {
        na_1 <- ")"
      }

      code_coverage <- c(code_coverage, paste0(
        pre, arg$name, " = sum(.ci_l_", arg$name, " <= ", arg$truth,
        " & ", arg$truth, " <= .ci_h_", arg$name, na_1,
        "/sum(!is.na(.ci_l_", arg$name, ") & !is.na(.ci_h_", arg$name,
        ") & !is.na(", arg$truth, ")", "),"))

      # } else if (stat_name == "correlation"){
      #
      # } else if (stat_name == "covariance"){
      #
      # }
    }
  }
  #
  #
  #   }
  #
  #   # for (arg_name in names(o_args)) {
  #   #   if (!(arg_name %in% metrics)) {
  #   #     stop(paste0(arg_name, " is an invalid summary metric."))
  #   #   }
  #   # }
  #   # for (metric in metrics) {
  #   #   if (!is.null(o_args[[metric]]) && !methods::is(o_args[[metric]][[1]],"list")) {
  #   #     o_args[[metric]] <- list(o_args[[metric]])
  #   #   }
  #   # }
  #
  # Parse code to display levels
  if (is.null(sim$levels$`no levels`)) {
    code_levels <- paste0("'",names_levels,"'=`",names_levels,"`[1],")
  } else {
    code_levels <- ""
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
    code_quantile,
    code_bias,
    code_bias_pct,
    code_mse,
    code_mae,
    code_coverage,
    code_correlation,
    code_covariance)
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))
  names(summary) <- gsub(pre, "", names(summary))

  return (summary)

}
