#' Summarize simulation results
#'
#' @description This function calculates summary statistics for simulation
#'     results, including descriptive statistics (e.g. measures of center or
#'     spread) and inferential statistics (e.g. bias or confidence interval
#'     coverage). All summary statistics are calculated over simulation
#'     replicates within a single simulation level.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param mc_se A logical argument indicating whether to compute Monte Carlo
#'     standard error and associated confidence interval for inferential summary
#'     statistics.  This applies only to the \code{bias}, \code{bias_pct},
#'     \code{mse}, \code{mae}, and \code{coverage} summary statistics.
#' @param ... One or more lists, separated by commas, specifying desired
#'     summaries of the \code{sim} simulation object. See examples. Each list
#'     must have a \code{stat} item, which specifies the type of summary
#'     statistic to be calculated. The \code{na.rm} item indicates whether to
#'     exclude \code{NA} values when performing the calculation (with default
#'     being \code{FALSE}). For \code{stat} options where the \code{name} item
#'     is optional, if it is not provided, a name will be formed from the type
#'     of summary and the column on which the summary is performed. Additional
#'     required items are detailed below for each \code{stat} type.
#'     \itemize{
#'
#'     \item{\code{list(stat="mean", x="col_1", name="mean_col", na.rm=F)}
#'     computes the mean of column \code{sim$results$col_1} for each level
#'     combination and creates a summary column named \code{"mean_col"}. Other
#'     single-column summary statistics (see the next few items) work
#'     analogously. \code{name} is optional.}
#'
#'     \item{\code{list(stat="median", ...)} computes the median.}
#'
#'     \item{\code{list(stat="var", ...)} computes the variance.}
#'
#'     \item{\code{list(stat="sd", ...)} computes the standard deviation.}
#'
#'     \item{\code{list(stat="mad", ...)} computes the mean absolute deviation.}
#'
#'     \item{\code{list(stat="iqr", ...)} computes the interquartile range.}
#'
#'     \item{\code{list(stat="min", ...)} computes the minimum.}
#'
#'     \item{\code{list(stat="max", ...)} computes the maximum.}
#'
#'     \item{\code{list(stat="is_na", ...)} computes the number of NA values.}
#'
#'     \item{\code{list(stat="correlation", x="col_1", y="col_2",
#'     name="cor_12")} computes the (Pearson's) correlation coefficient between
#'     \code{sim$results$col_1} and \code{sim$results$col_2} for each level
#'     combination and creates a summary column named \code{"cor_12"}.}
#'
#'     \item{\code{list(stat="covariance", x="col_1", y="col_2",
#'     name="cov_12")} computes the covariance between \code{sim$results$col_1}
#'     and \code{sim$results$col_2} for each level combination and creates a
#'     summary column named \code{"cov_12"}.}
#'
#'     \item{\code{list(stat="quantile", x="col_1", prob=0.8, name="q_col_1")}
#'     computes the 0.8 quantile of column \code{sim$results$col_1} and creates
#'     a summary column named \code{"q_col_1"}. \code{prob} can be any number in
#'     [0,1].}
#'
#'     \item{\code{list(stat="bias", estimate="est", truth=5,
#'     name="bias_est")} computes the absolute bias of the estimator
#'     corresponding to column \code{"sim$results$est"}, relative to the true
#'     value given in \code{truth}, and creates a summary column named
#'     \code{"bias_est"}. \code{name} is optional. See \emph{Details}.}
#'
#'     \item{\code{list(stat="bias_pct", estimate="est", truth=5,
#'     name="bias_est")} computes the percent bias of the estimator
#'     corresponding to column \code{"sim$results$est"}, relative to the true
#'     value given in \code{truth}, and creates a summary column named
#'     \code{"bias_pct_est"}. \code{name} is optional. See \emph{Details}.}
#'
#'     \item{\code{list(stat="mse", estimate="est", truth=5,
#'     name="mse_est")} computes the mean squared error of the estimator
#'     corresponding to column \code{"sim$results$est"}, relative to the true
#'     value given in \code{truth}, and creates a summary column named
#'     \code{"mse_est"}. \code{name} is optional. See \emph{Details}.}
#'
#'     \item{\code{list(stat="mae", estimate="est", truth=5,
#'     name="mae_est")} computes the mean absolute error of the estimator
#'     corresponding to column \code{"sim$results$est"}, relative to the true
#'     value given in \code{truth}, and creates a summary column named
#'     \code{"mae_est"}. \code{name} is optional. See \emph{Details}.}
#'
#'     \item{\code{list(stat="coverage", estimate="est", se="se_est",
#'     truth=5, name="cov_est")} or
#'     \code{list(stat="coverage", lower="est_l", upper="est_u",
#'     truth=5, name="cov_est")} computes confidence interval coverage. With the
#'     first form, \code{estimate} gives the name of the variable in
#'     \code{sim$results} corresponding to the estimator of interest and
#'     \code{se} gives the name of the variable containing the standard error of
#'     the estimator of interest. With the second form, \code{lower} gives the
#'     name of the variable containing the confidence interval lower bound and
#'     \code{upper} gives the name of the confidence interval upper bound. In
#'     both cases, \code{truth} is the true value (see \emph{Details}), and a
#'     summary column named \code{"cov_est"} is created.}
#'
#'    }
#' @details \itemize{
#'
#'     \item{For all inferential summaries there are three ways to specify
#'     \code{truth}: (1) a single number, meaning the estimand is the same
#'     across all simulation replicates and levels, (2) a numeric vector of the
#'     same length as the number of rows in \code{sim$results}, or (3) the name
#'     of a variable in \code{sim$results} containing the estimand of interest.}
#'
#'     \item{There are two ways to specify the confidence interval bounds for
#'     \code{coverage}. The first is to provide an \code{estimate} and its
#'     associated \code{se} (standard error). These should both be variables in
#'     \code{sim$results}. The function constructs a 95\% Wald-type confidence
#'     interval of the form \code{(estimate-1.96*se, estimate+1.96*se)}. The
#'     alternative is to provide \code{lower} and \code{upper} bounds, which
#'     should also be variables in \code{sim$results}. In this case, the
#'     confidence interval is (\code{lower}, \code{upper}). The coverage is the
#'     proportion of simulation replicates for a given level combination in
#'     which \code{truth} lies within the interval.}
#' }
#' @return A data frame containing the result of each specified summary function
#'     as a column, for each of the simulation levels. The column \code{n_reps}
#'     returns the number of successful simulation replicates within each level.
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
#'   list(stat = "mean", name="mean_lambda_hat", x="lambda_hat"),
#'   list(stat = "mse", name="lambda_mse", estimate="lambda_hat", truth=5)
#' )
#' @export
summarize <- function(sim, ..., mc_se = FALSE) {
  UseMethod("summarize")
}

#' @export
summarize.sim_obj <- function(sim, ..., mc_se = FALSE) {
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
               "covariance",
               "is_na")

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
  code_bias_mc_se <- ""
  code_bias_mc_cil <- ""
  code_bias_mc_ciu <- ""
  code_bias_pct <- ""
  code_bias_pct_mc_se <- ""
  code_bias_pct_mc_cil <- ""
  code_bias_pct_mc_ciu <- ""
  code_mse <- ""
  code_mse_mc_se <- ""
  code_mse_mc_cil <- ""
  code_mse_mc_ciu <- ""
  code_mae <- ""
  code_mae_mc_se <- ""
  code_mae_mc_cil <- ""
  code_mae_mc_ciu <- ""
  code_coverage <- ""
  code_coverage_mc_se <- ""
  code_coverage_mc_cil <- ""
  code_coverage_mc_ciu <- ""
  code_correlation <- ""
  code_covariance <- ""
  code_is_na <- ""


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
      if (is.null(arg$name)) {
        arg$name <- paste0("quantile_", arg$prob, "_", arg$x)
      }

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
        pre, arg$name, " = tryCatch(quantile(", arg$x, ", probs=", arg$prob,
        ",", na_1, " error = function(e) {return(NA)}),"
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
      handle_errors(arg$estimate, "is.null",
                    msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,
                               "` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth,
                    "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_bias <- c(code_bias, paste0(
        pre, arg$name, " = mean(", arg$estimate, "-", arg$truth, na_1
      ))

      if (mc_se){
        if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
          na_1 <- ", na.rm=TRUE)"
        } else {
          na_1 <- ")"
        }
        code_bias_mc_se <- c(code_bias_mc_se, paste0(
          pre, arg$name, "_mc_se = sqrt((1/(sum(!is.na(", arg$estimate,
          "))-1))*mean((",
          arg$estimate, "-", arg$truth, "-", pre, arg$name, ")^2", na_1, "),"
        ))
        code_bias_mc_cil <- c(code_bias_mc_cil, paste0(
          pre, arg$name, "_mc_ci_l = ", pre, arg$name, "- 1.96*", pre, arg$name,
          "_mc_se,"
        ))
        code_bias_mc_ciu <- c(code_bias_mc_ciu, paste0(
          pre, arg$name, "_mc_ci_u = ", pre, arg$name, "+ 1.96*", pre, arg$name,
          "_mc_se,"
        ))
      }

      # parse bias pct code
    } else if (stat_name == "bias_pct"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("bias_pct_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null",
                    msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,
                               "` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth,
                    "` is neither a number nor a variable in results"))
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

      if (mc_se){
        if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
          na_1 <- ", na.rm=TRUE)"
        } else {
          na_1 <- ")"
        }
        code_bias_pct_mc_se <- c(code_bias_mc_se, paste0(
          pre, arg$name, "_mc_se = (1/abs(", arg$truth,
          "[1]))*sqrt((1/(sum(!is.na(", arg$estimate,"))-1))*mean((",
          arg$estimate, "-", arg$truth, "-", pre, arg$name, ")^2", na_1, "),"
        ))
        code_bias_pct_mc_cil <- c(code_bias_pct_mc_cil, paste0(
          pre, arg$name, "_mc_ci_l = ", pre, arg$name, "- 1.96*", pre, arg$name,
          "_mc_se,"
        ))
        code_bias_pct_mc_ciu <- c(code_bias_pct_mc_ciu, paste0(
          pre, arg$name, "_mc_ci_u = ", pre, arg$name, "+ 1.96*", pre, arg$name,
          "_mc_se,"
        ))
      }

      # parse MSE code
    } else if (stat_name == "mse"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("MSE_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null",
                    msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,
                               "` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth,
                    "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mse <- c(code_mse, paste0(
        pre, arg$name, " = mean((", arg$estimate, "-", arg$truth, ")^2", na_1
      ))

      if (mc_se){
        if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
          na_1 <- ", na.rm=TRUE)"
        } else {
          na_1 <- ")"
        }
        code_mse_mc_se <- c(code_mse_mc_se, paste0(
          pre, arg$name, "_mc_se = sqrt((1/(sum(!is.na(", arg$estimate,
          "))-1))*mean(((",
          arg$estimate, "-", arg$truth, ")^2 -", pre, arg$name, ")^2", na_1,
          "),"
        ))
        code_mse_mc_cil <- c(code_mse_mc_cil, paste0(
          pre, arg$name, "_mc_ci_l = ", pre, arg$name, "- 1.96*", pre, arg$name,
          "_mc_se,"
        ))
        code_mse_mc_ciu <- c(code_mse_mc_ciu, paste0(
          pre, arg$name, "_mc_ci_u = ", pre, arg$name, "+ 1.96*", pre, arg$name,
          "_mc_se,"
        ))
      }

      # parse MAE code
    } else if (stat_name == "mae"){

      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("MAE_", arg$estimate) }

      # Handle errors
      handle_errors(arg$estimate, "is.null",
                    msg="`estimate` argument is required")
      handle_errors(arg$truth, "is.null", msg="`truth` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$estimate, "is.in", other=names(R),
                    msg=paste0("`",arg$estimate,
                               "` is not a variable in results"))
      handle_errors(R[[arg$estimate]], "is.numeric.vec", name=arg$estimate)
      if (length(arg$truth)>1 ||
          (!is.numeric(arg$truth) && !(arg$truth %in% names(R))) ||
          (arg$truth %in% names(R) && !is.numeric(R[[arg$truth]]))) {
        stop(paste0("`", arg$truth,
                    "` is neither a number nor a variable in results"))
      }

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", na.rm=TRUE),"
      } else {
        na_1 <- "),"
      }

      code_mae <- c(code_mae, paste0(
        pre, arg$name, " = mean(abs(", arg$estimate, "-", arg$truth, ")", na_1
      ))

      if (mc_se){
        if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
          na_1 <- ", na.rm=TRUE)"
        } else {
          na_1 <- ")"
        }
        code_mae_mc_se <- c(code_mae_mc_se, paste0(
          pre, arg$name, "_mc_se = sqrt((1/(sum(!is.na(", arg$estimate,
          "))-1))*mean((abs(", arg$estimate, "-", arg$truth, ") -", pre,
          arg$name, ")^2", na_1, "),"
        ))
        code_mae_mc_cil <- c(code_mae_mc_cil, paste0(
          pre, arg$name, "_mc_ci_l = ", pre, arg$name, "- 1.96*", pre, arg$name,
          "_mc_se,"
        ))
        code_mae_mc_ciu <- c(code_mae_mc_ciu, paste0(
          pre, arg$name, "_mc_ci_u = ", pre, arg$name, "+ 1.96*", pre, arg$name,
          "_mc_se,"
        ))
      }

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
        stop(paste0("`", arg$truth,
                    "` is neither a number nor a variable in results"))
      }
      if (!((!is.null(arg$est) && !is.null(arg$se)) ||
            (!is.null(arg$lower) && !is.null(arg$upper)))) {
        stop(paste0("Either `estimate` and `se` OR `lower` and `upper` must be",
                    " provided"))
      }

      # Handle case where user provides estimate+se
      if (!is.null(arg$se) && !is.null(arg$estimate)) {
        handle_errors(arg$estimate, "is.in", other=names(R),
                      msg=paste0("`",arg$estimate,
                                 "` is not a variable in results"))
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
                      msg=paste0("`",arg$lower,
                                 "` is not a variable in results"))
        handle_errors(arg$upper, "is.in", other=names(R),
                      msg=paste0("`",arg$upper,
                                 "` is not a variable in results"))
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

      if (mc_se){
        if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
          na_1 <- ", na.rm=TRUE)"
        } else {
          na_1 <- ")"
        }

        na_code <- paste0("sum(!is.na(.ci_l_", arg$name, ") & !is.na(.ci_h_",
                          arg$name, ") & !is.na(", arg$truth, ")", ")")
        code_coverage_mc_se <- c(code_coverage_mc_se, paste0(
          pre, arg$name, "_mc_se = sqrt((1/", na_code, ")*", pre, arg$name,
          "* (1 - ", pre, arg$name, ")),"
        ))
        code_coverage_mc_cil <- c(code_coverage_mc_cil, paste0(
          pre, arg$name, "_mc_ci_l = max(", pre, arg$name, "- 1.96*", pre,
          arg$name, "_mc_se, 0),"
        ))
        code_coverage_mc_ciu <- c(code_coverage_mc_ciu, paste0(
          pre, arg$name, "_mc_ci_u = min(", pre, arg$name, "+ 1.96*", pre,
          arg$name, "_mc_se, 1),"
        ))
      }

    } else if (stat_name == "correlation"){
      # if name missing, create a name
      handle_errors(arg$name, "is.null", msg="`name` argument is required")

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$y, "is.null", msg="`y` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(arg$y, "is.in", other=names(R),
                    msg=paste0("`",arg$y,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)
      handle_errors(R[[arg$y]], "is.numeric.vec", name=arg$y)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", use='complete.obs'),"
      } else {
        na_1 <- "),"
      }

      code_correlation <- c(code_correlation, paste0(
        pre, arg$name, " = cor(", arg$x, ",", arg$y, na_1
      ))
    } else if (stat_name == "covariance"){
      handle_errors(arg$name, "is.null", msg="`name` argument is required")

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$y, "is.null", msg="`y` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(arg$y, "is.in", other=names(R),
                    msg=paste0("`",arg$y,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)
      handle_errors(R[[arg$y]], "is.numeric.vec", name=arg$y)

      if (!is.null(arg$na.rm) && arg$na.rm==TRUE) {
        na_1 <- ", use='complete.obs'),"
      } else {
        na_1 <- "),"
      }

      code_covariance <- c(code_covariance, paste0(
        pre, arg$name, " = cov(", arg$x, ",", arg$y, na_1
      ))
    } else if (stat_name == "is_na"){
      # if name missing, create a name
      if (is.null(arg$name)) { arg$name <- paste0("is_na_", arg$x) }

      # Handle errors
      handle_errors(arg$x, "is.null", msg="`x` argument is required")
      handle_errors(arg$name, "is.character", name="name")
      handle_errors(arg$x, "is.in", other=names(R),
                    msg=paste0("`",arg$x,"` is not a variable in results"))
      handle_errors(R[[arg$x]], "is.numeric.vec", name=arg$x)

      code_is_na <- c(code_is_na, paste0(
        pre, arg$name, " = sum(is.na(", arg$x, ")),"
      ))
    }
  }

  code_nrep <- "n_reps = dplyr::n(),"

  ### Put code strings together
  summarize_code <- c(
    "as.data.frame(dplyr::summarize(dplyr::group_by(R, level_id),",
    code_levels,
    code_nrep,
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
    code_bias_mc_se,
    code_bias_mc_cil,
    code_bias_mc_ciu,
    code_bias_pct,
    code_bias_pct_mc_se,
    code_bias_pct_mc_cil,
    code_bias_pct_mc_ciu,
    code_mse,
    code_mse_mc_se,
    code_mse_mc_cil,
    code_mse_mc_ciu,
    code_mae,
    code_mae_mc_se,
    code_mae_mc_cil,
    code_mae_mc_ciu,
    code_coverage,
    code_coverage_mc_se,
    code_coverage_mc_cil,
    code_coverage_mc_ciu,
    code_correlation,
    code_covariance,
    code_is_na)
  summarize_code <- c(summarize_code, "))")
  summary <- eval(parse(text=summarize_code))
  names(summary) <- gsub(pre, "", names(summary))

  return (summary)

}
