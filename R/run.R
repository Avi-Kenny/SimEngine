#' Run the simulation
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param script The name of a simulation script, added using add_script()
#' @examples
#' !!!!! TO DO
#' @export
run <- function(sim_obj, script) UseMethod("run")

#' @export
run.simba <- function(sim_obj, script) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  # Set up levels_grid
  sim_obj$levels[["sim_id"]] <- 1:sim_obj$config$num_sim
  levels_grid <- expand.grid(sim_obj$levels, stringsAsFactors=FALSE)
  levels_names <- names(levels_grid)
  levels_grid <- cbind(1:nrow(levels_grid), levels_grid)
  names(levels_grid) <- c("sim_uid",levels_names)

  # Load creators and methods
  for (obj in c("creators", "methods")) {
    for (i in 1:length(sim_obj[[obj]])) {
      assign(
        x = names(sim_obj[[obj]])[i],
        value = (sim_obj[[obj]])[[i]]
      )
    }
  }


  # !!!!! Temporary cluster code (1 of 2): START
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  cluster_export <- c("sim_obj", "levels_grid", "use_method")
  for (obj in c("creators", "methods")) {
    for (i in 1:length(sim_obj[[obj]])) {
      cluster_export <- c(cluster_export, names(sim_obj[[obj]])[i])
    }
  }
  envir <- environment()
  clusterExport(cl, cluster_export, envir)
  # !!!!! Temporary cluster code (1 of 2): END


  # Run simulations
  results_lists <- parLapply(cl, 1:nrow(levels_grid), function(i) {
  # results_lists <- lapply(1:nrow(levels_grid), function(i) {

    print("check 3")
    print("sim_obj")
    print(sim_obj$config$datasets)

    # Set up levels row and run script
    L <- levels_grid[i,]

    # !!!!! This is janky AF. Use environments properly
    eval(parse(text=c("s_copy <-", deparse(sim_obj$scripts[[script]]))))
    eval(parse(text=c("use_method <-", deparse(use_method))))

    # script_results <- sim_obj$scripts[[script]](as.list(L))
    script_results <- do.call(
      what = s_copy,
      args = list(as.list(L)) # !!!!! This may throw a warning if script does not take any arguments
    )

    return (list(
      "sim_uid" = i,
      "results" = script_results
    ))

  })


  # !!!!! Temporary cluster code (2 of 2): START
  stopCluster(cl)
  # !!!!! Temporary cluster code (2 of 2): END


  # Convert summary statistics to data frame
  results_df <- data.frame(
    matrix(
      unlist(results_lists),
      nrow = length(results_lists),
      byrow = TRUE
    )
  )
  names(results_df) <- c("sim_uid", names(results_lists[[1]]$results))

  # # Join `results` with `levels_grid`
  results_df <- dplyr::inner_join(levels_grid, results_df, by="sim_uid")

  # Return results
  return (results_df)

}
