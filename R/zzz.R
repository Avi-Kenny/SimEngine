
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Welcome to simba! Full package documentation can be found at:\n",
    "https://avi-kenny.github.io/simba"
  ))
}
