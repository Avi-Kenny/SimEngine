#' Create a new simulation object
#'
#' @description Create a new simulation object. This is typically the first
#'     function to be called when running a simulation using \pkg{SimEngine}. Most
#'     other \pkg{SimEngine} functions take a simulation object as their first
#'     argument.
#' @return A simulation object, of class \code{sim_obj}
#' @seealso
#' Visit \url{https://avi-kenny.github.io/SimEngine/} for more information on how to
#'     use the \pkg{SimEngine} simulation framework.
#' @examples
#' sim <- new_sim()
#' print(sim)
#' @export
new_sim <- function() {

  # Simulation initial seed
  ..seed <- as.integer(1e9*runif(1))

  # Reference to the global environment
  ..e <- .GlobalEnv

  # Simulation object initial state
  ..sim <- list(

    # Simulation configuration; see set_config() docs
    config = list(
      num_sim = 1,
      parallel = FALSE,
      n_cores = NA,
      packages = NULL,
      stop_at_error = FALSE,
      seed = ..seed,
      progress_bar = TRUE,
      batch_levels = NA,
      return_batch_id = F
    ),

    # Simulation levels; see set_levels() docs
    levels = list("no_levels"=TRUE),

    # Simulation level grid; see set_levels() docs
    levels_grid = data.frame(level_id=1, no_levels=T),

    # Container for simulation results; this will be a dataframe if there is at
    #     least one successfully-run simulation replicate after running/updating
    #     the simulation
    results = "Simulation has not been run yet.",

    # Container for complex-type simulation results; this will be a named list
    #     if the simulation involves complex return types with keys of the form
    #     "sim_uid_123", where 123 is the sim_uid
    results_complex = list(),

    # Container for simulation errors; this will be a dataframe if there is at
    #     least one error after running/updating the simulation
    errors = "Simulation has not been run yet.",

    # Container for simulation warnings; this will be a dataframe if there is at
    #     least one warning after running/updating the simulation
    warnings = "Simulation has not been run yet.",

    # Internal objects that the user should not ever interact with
    internals = list(

      # Character vector of level names
      level_names = c("no_levels"),

      # Boolean vector; stores whether level is a list (TRUE) or not (FALSE)
      levels_types = FALSE,

      # Identical to sim$levels, except list-type levels are converted to
      #     character vectors based on level names
      levels_shallow = list("no_levels"=TRUE),

      # A dataframe of the same structure as sim$levels but in which rows are
      #     never removed
      levels_grid_historical = data.frame(level_id=1),

      # A dataframe created and updated by the update_sim_uid_grid() function,
      #     which stores information about individual simulation replicates
      sim_uid_grid = data.frame(
        sim_uid = 1,  # Unique identifier for a single simulation replicate
        level_id = 1, # The corresponding level_id (maps to sim$levels_grid)
        rep_id = 1,   # Replicate identifier (within a level)
        active = T,   # If F, this replicate has been removed, via set_levels()
                      #     or set_config()
        to_run = T,   # If T, this replicate has not yet been run
        batch_id = 1, # Identifier for a replicate batch; see batch()
        core_id = 1   # Replicates with to_run==T and a common core_id will be
                      #     run on a single core
      ),

      # A dataframe with two columns mapping level_id to batch_id_pre
      level_batch_map = data.frame(),

      # Task ID; used by cluster_execute()
      tid = NA,

      # Name of the variable that the simulation object is assigned to; used
      #     by cluster_execute()
      sim_var = "",

      # A flag temporarily set by update_sim() and checked by run() that denotes
      #     whether the simulation is currently being updated; this reverts back
      #     to FALSE when the update is done
      update_sim = FALSE,

      # Stores a reference to the environment in which new_sim() is called
      env_calling = parent.frame()

    ),

    # Variables that the user may want to interact with
    vars = list(

      # The simulation seed; see set_config() docs
      seed = ..seed,

      # The environment in which the simulation script is run; all needed
      #     objects will be automatically added to this environment
      env = new.env(),

      # An integer that represents the number of replicates need to run; note
      #     that this may be inaccurate if update_sim() is run with
      #     keep_errors=F
      num_sim_total = 1,

      # A character string representing the "run state" of the simulation
      run_state = "pre run",

      # Results of a call to sessionInfo(); this is updated by set_config()
      #     after packages are loaded
      session_info = utils::sessionInfo()

    ),

    # Simulation script; see set_script() docs
    script = NULL
  )

  # Set flag denoting that sim_uid_grid is blank; this is checked by
  #     update_sim_uid_grid
  attr(..sim$internals$sim_uid_grid, "blank") <- T

  # Create batch_cache and set initial values; see batch() docs
  assign(x="..batch_cache", value=new.env(), envir=..sim$vars$env)
  assign(x="batch_levels", value=NA,
         envir=get(x="..batch_cache", envir=..sim$vars$env))

  # A boolean flag that is checked by the batch() function to ensure that
  #     n_cores is set via set_config() if the batch() function is being used
  #     when running on on a cluster computing system; necessary because batch()
  #     doesn't take sim as an argument
  assign(x="..flag_batch_n_cores", value=F, envir=..sim$vars$env)

  # A boolean flag that is checked by the batch() function to disallow
  #     simultaneous use of batch() and update_sim(); necessary because batch()
  #     doesn't take sim as an argument
  assign(x="..flag_batch_update", value=F, envir=..sim$vars$env)

  # A (hidden) global reference to the simulation environment that can be
  #     searched for via get() by functions (currently use_method and batch)
  #     that need to access this environment but don't take sim as an argument
  assign(x="..env", value=..sim$vars$env, envir=..e)

  # Simulation objects have the class "sim_obj"
  class(..sim) <- "sim_obj"
  rm(..e, ..seed)

  return (..sim)

}
