
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Welcome to SimEngine! Full package documentation can be found at:\n",
    "https://avi-kenny.github.io/SimEngine"
  ))
}
