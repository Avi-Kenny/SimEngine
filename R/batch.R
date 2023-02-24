#' Run a block of code as part of a batch
#'
#' @description This function is designed to be used within a simulation script
#'     to leverage "replicate batches". This is useful if you want to share data
#'     or objects between simulation replicates. Essentially, it allows you to
#'     take your simulation replicates and divide them into "batches"; all
#'     replicates in a given batch will then share a single set of objects. The
#'     most common use case for this is if you have a simulation that involves
#'     generating one dataset, analyzing it using multiple methods, and then
#'     repeating this a number of times. See
#'     \url{https://avi-kenny.github.io/SimEngine/advanced-usage/#using-the-batch-function}
#'     for a thorough overview of how this function is used.
#' @param code A block of code enclosed by curly braces {}; see examples.
#' @examples
#' sim <- new_sim()
#' create_data <- function(n, mu) { rnorm(n=n, mean=mu) }
#' est_mean <- function(dat, type) {
#'   if (type=="est_mean") { return(mean(dat)) }
#'   if (type=="est_median") { return(median(dat)) }
#' }
#' sim %<>% set_levels(n=c(10,100), mu=c(3,5), est=c("est_mean","est_median"))
#' sim %<>% set_config(
#'   num_sim = 2,
#'   batch_levels = c("n","mu"),
#'   return_batch_id = TRUE
#' )
#' sim %<>% set_script(function() {
#'   batch({
#'     dat <- create_data(n=L$n, mu=L$mu)
#'   })
#'   mu_hat <- est_mean(dat=dat, type=L$est)
#'   return(list(
#'     "mu_hat" = round(mu_hat,2),
#'     "dat_1" = round(dat[1],2)
#'   ))
#' })
#' sim %<>% run()
#' sim$results[order(sim$results$batch_id),]
#' @export
batch <- function(code) {

  ..env <- get("..env", envir=.GlobalEnv)
  ..cache <- get(x="..batch_cache", envir=..env)

  # Handle errors
  handle_errors(get("batch_levels", envir=..cache)[1], "is.na", msg=paste0(
    "If the batch() function is used, you must set the `batch_levels` config o",
    "ption via set_config()"
  ))
  if (get(x="..flag_batch_n_cores", envir=..env)) {
    stop(paste0("If the batch() function is used on a cluster computing system",
                ", you must set the `n_cores` config option via set_config()"))
  }
  if (get(x="..flag_batch_update", envir=..env)) {
    stop(paste0("You cannot add replicates to a simulation that uses the batch",
                "() function"))
  }

  batch_id <- get("L", envir=parent.frame())$batch_id
  objs <- ..cache[[as.character(batch_id)]]
  if (is.null(objs)) {
    objs_pre <- ls(envir=parent.frame(), all.names=T)
    ..code <- substitute(code)
    rm(code)
    eval(..code, envir=parent.frame())
    objs_post <- ls(envir=parent.frame(), all.names=T)
    objs_diff <- objs_post[!(objs_post %in% objs_pre)]
    objs <- new.env()
    if (length(objs_diff)!=0) {
      for (i in c(1:length(objs_diff))) {
        objs[[objs_diff[i]]] <- get(objs_diff[i], envir=parent.frame())
      }
    } else {
      warning("No new objects were created within batch(); see documentation")
    }
    ..cache[[as.character(batch_id)]] <- objs
  } else {
    ls_objs <- ls(objs, all.names=T)
    if (length(ls_objs)!=0) {
      for (i in c(1:length(ls_objs))) {
        key <- ls_objs[i]
        val <- get(ls_objs[i], objs)
        assign(key, val, envir=parent.frame())
      }
    } else {
      warning("No new objects were created within batch(); see documentation")
    }
  }

}
