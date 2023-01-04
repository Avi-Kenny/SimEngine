#' Wrapper to run a block of code as part of a batch
#'
#' @description TO DO
#' @return TO DO
#' @examples
#' TO DO
#' @export
batch <- function(code) {

  ..env <- get("..env", envir=.GlobalEnv)
  ..cache <- get(x="..batch_cache", envir=..env)

  handle_errors(get("batch_levels", envir=..cache), "is.na", msg=paste0(
    "If the batch() function is used, you must set the `batch_levels` config o",
    "ption via set_config()"
  ))

  browser() # !!!!!

  # Get error flags
  batch_flag_n_cores <- get(x="batch_flag_n_cores",
                            envir=get(x="..batch_cache", envir=..env))

  if (batch_flag_n_cores) {
    stop(paste0("If the batch() function is used on a cluster computing system",
                ", you must set the `n_cores` config option via set_config()"))
  }

  # TO DO: throw error/warning if update is being used

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
