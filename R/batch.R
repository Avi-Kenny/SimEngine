#' Wrapper to run a block of code as part of a batch
#'
#' @description TO DO
#' @return TO DO
#' @examples
#' TO DO
#' @export
batch <- function(code, warn=T) {

  ..env <- get("..env", envir=.GlobalEnv)
  ..cache <- get("..batch_cache", envir=.GlobalEnv)

  handle_errors(get("batch", envir=..cache), "is.null", msg=paste0(
    "If the batch() function is used, you must set the `batch` config option via",
    " set_config()"
  ))

  if (warn) {
    # TO DO: throw warning if update is being used
  }

  # TO DO: Need a way to clear the cache after everything runs

  batch_id <- L$batch_id
  objs <- ..cache[[as.character(batch_id)]]
  if (is.null(objs)) {
    objs_pre <- ls(envir=parent.frame())
    ..code <- substitute(code)
    rm(code)
    ..env_cl <- new.env()
    eval(..code, envir=parent.frame())
    objs_post <- ls(envir=parent.frame())
    objs_diff <- objs_post[!(objs_post %in% objs_pre)]
    objs <- new.env()
    for (i in c(1:length(objs_diff))) {
      objs[[objs_diff[i]]] <- get(objs_diff[i], envir=parent.frame())
    }
    ..cache[[as.character(batch_id)]] <- objs
  } else {
    for (i in c(1:length(ls(objs)))) {
      key <- ls(objs)[i]
      val <- get(ls(objs)[i], objs)
      assign(key, val, envir=parent.frame())
    }
  }

}
