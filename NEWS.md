# SimEngine 1.2.1

### Minor changes

- Fixed a bug associated with function scoping; see [https://github.com/Avi-Kenny/SimEngine/issues/92](https://github.com/Avi-Kenny/SimEngine/issues/92).
- Added an option for the `summarize()` function to calculate Monte Carlo standard errors and confidence intervals for all inferential summary statistics. For summary statistics meant to estimate a population
quantity (e.g., the bias of an estimator), the Monte Carlo standard error quantifies variability due to running
a finite number of simulation replicates. 

# SimEngine 1.2.0

### Major changes

- Added the `batch()` function, which allows for sharing of data or objects between simulation replicates. Essentially, it allows you to take your simulation replicates and divide them into "batches"; all replicates in a given batch will then share a single set of objects. The most common use case for this is if you have a simulation that involves generating one dataset, analyzing it using multiple methods, and then repeating this a number of times. See the documentation for more info.
- Overhauled and simplified the interface of the `summarize()` function and added several summary metrics (correlation, covariance, n_reps); see the documentation for more info.
- Users can now run multiple simulation replicates per core when running code on a CCS using `run_on_cluster()`.

### Minor changes

- Added results of a call to `sessionInfo()` to `vars()`.
- Changed `sim_id` to `rep_id` to avoid confusion with `sim_uid`.
- Removed the option for a user to specify a subset of `sim_uids`.
- Removed the `keep_extra` option from `update_sim()` and `update_sim_on_cluster()`.
- Removed the `.add` option from `set_levels()`.
- Added a constraint to `set_levels()` to prevent changing of level variables once they are initially set.
- Fixed a bug associated with functions that have a NULL environment (e.g. as.integer).
- Fixed bugs that previously prevented users from running a simulation with no levels.
- Various minor bug fixes.

# SimEngine 1.1.0

### Major changes

- Removed the add_* functions, as we ultimately deemed these unnecessary to the workflow: `add_creator()`, `add_method()`, `add_constants()`. Instead, functions declared in the parent frame of the `new_sim()` call are automatically added to the simulation object. It is now recommended that simulation constants are stored and referenced in the same way as levels; see "Advanced Usage" on the SimEngine website ([https://avi-kenny.github.io/SimEngine/](https://avi-kenny.github.io/SimEngine/)).

### Minor changes

- Fixed a bug related to the use of closures added via `add_method()`.
- Added more info in the documentation about how to properly load packages via `set_config()` (since this was a source of confusion among some users).
- Fixed a few minor issues with the documentation.

# SimEngine 1.0.0

### Major changes

- Initial package release

### Minor changes

- None
