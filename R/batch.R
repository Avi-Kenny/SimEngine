#' Wrapper to run a block of code as part of a batch
#'
#' @description TO DO
#' @return TO DO
#' @examples
#' TO DO
#' @export
batch <- function(code, warn=T) {

  # ..env <- get("..env", envir=.GlobalEnv)
  ..cache <- get("..batch_cache", envir=.GlobalEnv)

  handle_errors(get("batch_levels", envir=..cache), "is.na", msg=paste0(
    "If the batch() function is used, you must set the `batch_levels` config option via",
    " set_config()"
  ))

  if (warn) {
    # TO DO: throw warning if update is being used; maybe throw error
  }

  batch_id <- get("L", envir=parent.frame())$batch_id
  objs <- ..cache[[as.character(batch_id)]]
  if (is.null(objs)) {
    objs_pre <- ls(envir=parent.frame())
    ..code <- substitute(code)
    rm(code)
    # ..env_cl <- new.env()
    eval(..code, envir=parent.frame())
    objs_post <- ls(envir=parent.frame())
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
    if (length(ls(objs))!=0) {
      for (i in c(1:length(ls(objs)))) {
        key <- ls(objs)[i]
        val <- get(ls(objs)[i], objs)
        assign(key, val, envir=parent.frame())
      }
    } else {
      warning("No new objects were created within batch(); see documentation")
    }
  }

}
