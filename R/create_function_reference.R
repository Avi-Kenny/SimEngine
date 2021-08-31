
# This file will update the "function reference" section of the SimEngine
#     website (https://avi-kenny.github.io/SimEngine/function-reference)
# !!!!! Run this file manually for now (eventually automate it via devtools)

if (FALSE) {

  library(tools)
  library(magrittr)

  fns <- dir("man")[!(dir("man") %in% c("figures", "SimEngine.Rd"))]
  fns <- substring(fns, 1, nchar(fns)-3)
  fn_ref_path <- "docs/docs/function-reference/"

  # Delete old files
  for (fn in dir(fn_ref_path)) {
    if (!(fn %in% c("function-reference.md"))) {
      unlink(paste0(fn_ref_path, fn))
    }
  }

  for (i in 1:length(fns)) {

    # Variables
    fn <- fns[i]
    if (substr(fn, start = nchar(fn) - 5, stop = nchar(fn)) == ".sim_obj"){
      fn2 <- substr(fn, start = 1, stop = nchar(fn) - 6)
    } else{
      fn2 <- fn
    }
    path_rd <- paste0("man/", fn, ".Rd")
    path_html <- paste0(fn_ref_path, fn2, ".html")
    path_md <- paste0(fn_ref_path, fn2, ".md")

    # Convert Rd file to HTML
    Rd2HTML(
      Rd = parse_Rd(path_rd),
      out = path_html,
      package = "SimEngine"
    )

    # Read in HTML, line by line
    cxn <- file(path_html, open="r")
    lines <- readLines(cxn)

    # Create new file
    sink(path_md)

    # Write Jekyll header
    cat("---\n")
    cat("layout: page\n")
    cat(paste("title:", fn2, "\n"))
    cat(paste("nav_order:", i, "\n"))
    cat(paste0("permalink: /function-reference/", fn2, "/\n"))
    cat("parent: Function reference\n")
    cat("---\n\n")

    # Rewrite/truncate HTML fragment
    for (j in 1:length(lines)) {

      line <- lines[j]

      # Skip first four lines
      if (j>=5) {

        # Make replacements
        line %<>% sub("<pre>", "```R", ., fixed=T)
        line %<>% sub("</pre>", "```", ., fixed=T)
        line %<>% gsub("&lt;", "<", ., fixed=T)
        line %<>% gsub("&gt;", ">", ., fixed=T)
        line %<>% gsub("<code>", "<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>", ., fixed=T)
        line %<>% gsub("</code>", "</span>", ., fixed=T)

        # Write lines
        if (line != "</body></html>") {
          cat(line)
          cat("\n")
        }

      }

    }

    # Close connections
    sink()
    close(cxn)

    # Delete original HTML file
    unlink(path_html)

  }

}
