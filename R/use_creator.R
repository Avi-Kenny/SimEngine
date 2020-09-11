#' Use a particular creator within a simulation
#'
#' @param creator TO DO
#' @return Runs the creator specified by the first argument within a simulation,
#'     supplying all additional arguments to the creator
#' @examples
#' !!!!! TO DO
#' @export
use_creator <- function(creator, ...) {

  # !!!!! Note the reference to sim_obj is currently global
  if (length(list(...))>0) {
    result <- do.call(sim_obj$creators[[creator]], list(...))
  } else {
    result <- do.call(sim_obj$creators[[creator]])
  }

  return (result)

}
