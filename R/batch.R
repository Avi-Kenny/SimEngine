#' Wrapper to run a block of code as part of a batch
#'
#' @description TO DO
#' @return TO DO
#' @examples
#' TO DO
#' @export
batch <- function(code, warn=T) {

  ..cache <- get(x="..batch_cache", envir=get("..env", envir=.GlobalEnv))

  # ..env <- get("..env", envir=.GlobalEnv)
  # ..cache <- get("..batch_cache", envir=.GlobalEnv)

  handle_errors(get("batch_levels", envir=..cache), "is.na", msg=paste0(
    "If the batch() function is used, you must set the `batch_levels` config option via",
    " set_config()"
  ))

  # # !!!!! Get reference to sim and then run this; or set a hidden flag in ..env
  # if (sim$config$parallel=="cluster" && is.na(sim$config$n_cores)) {
  #   stop("You must specify n_cores in set_config() if using batch on cluster")
  # }

  if (warn) {
    # TO DO: throw warning if update is being used; maybe throw error
  }

  batch_id <- get("L", envir=parent.frame())$batch_id
  objs <- ..cache[[as.character(batch_id)]]
  if (is.null(objs)) {
    objs_pre <- ls(envir=parent.frame(), all.names=T)
    ..code <- substitute(code)
    rm(code)
    # ..env_cl <- new.env()
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
