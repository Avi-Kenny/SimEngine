# SimEngine 1.2.0

### Major changes

- Overhauled the interface of the `summarize()` function; see the function reference
- TO DO

### Minor changes

- TO DO
- TO DO

# SimEngine 1.1.0

### Major changes

- Removed the add_* functions, as we ultimately deemed these unnecessary to the workflow: `add_creator()`, `add_method()`, `add_constants()`. Instead, functions declared in the parent frame of the `new_sim()` call are automatically added to the simulation object. It is now recommended that simulation constants are stored and referenced in the same way as levels; see "Advanced Usage" on the SimEngine website (https://avi-kenny.github.io/SimEngine).

### Minor changes

- Fixed a bug related to the use of closures added via `add_method()`.
- Added more info in the documentation about how to properly load packages via `set_config()` (since this was a source of confusion among some users).
- Fixed a few minor issues with the documentation.

# SimEngine 1.0.0

### Major changes

- Initial package release

### Minor changes

- None
