#' Use a particular method within a simulation
#'
#' @return Runs the method specified by the first argument within a simulation,
#'     supplying all additional arguments to the method
#' @examples
#' !!!!! TO DO
#' @export
use_method <- function(method, ...) {

  # !!!!! Note the reference to sim_obj is currently global
  if (length(list(...))>0) {
    result <- do.call(sim_obj$methods[[method]], list(...))
  } else {
    result <- do.call(sim_obj$methods[[method]])
  }

  return (result)

}
