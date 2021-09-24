## Resubmission
This is a resubmission. The following changes were made:

**Unexecutable code in man/run_on_cluster.Rd and man/update_sim_on_cluster.Rd**
The "unexecutable code" is sample BASH code for running simulations on cluster computing environments (a key feature of this package). To avoid these warnings and make it clear that this is not R code, these lines were commented out, and additional documentation was added specifying the purpose of this code.

**You write information messages to the console that cannot be easily suppressed...Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console (except for print, summary, interactive functions).**
We have replaced all calls to cat() with calls to message(), other than in one S3 print() method. We removed a feature that used cat() to create a custom output file, as this was unnecessary. Additionally, we added a configuration option to SimEngine::set_config() to suppress the progress bar (from the pbapply package).

**Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.**
Only one SimEngine function uses setwd(), but it is not called by default; it is only called if the user specifies a directory that he/she wants to use. We also added oldwd<-getwd();on.exit(setwd(oldwd)); to this function, as recommended. No other user options or graphical parameters are changed.

**Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.**
We confirm that we are not using more than 2 cores in any examples, vignettes, etc.

## Test environments
* local Windows 10  install, R 4.1.0
* Windows 10, R 4.1.1 (GitHub actions)
* macOS X, R 4.1.1 (GitHub actions)
* ubuntu 20.04, R 4.1.1 (GitHub actions)

## R CMD check results
There were no ERRORs, no WARNINGs, and no NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package. 
