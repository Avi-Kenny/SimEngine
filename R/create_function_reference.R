
# This file will update the "function reference" section of the simba website (https://avi-kenny.github.io/simba/function-reference)
# !!!!! Run this file manually for now (eventually automate it via devtools)

if (FALSE) {

  library(tools)
  library(magrittr)

  fns <- dir("man")[dir("man")!="figures"]
  fns <- substring(fns, 1, nchar(fns)-3)

  for (i in 1:length(fns)) {

    # Variables
    fn <- fns[i]
    path_rd <- paste0("man/", fn, ".Rd")
    path_html <- paste0("docs/docs/function-reference/", fn, ".html")
    path_md <- paste0("docs/docs/function-reference/", fn, ".md")

    # Convert Rd file to HTML
    Rd2HTML(
      Rd = parse_Rd(path_rd),
      out = path_html,
      package = "simba"
    )

    # Read in HTML, line by line
    cxn <- file(path_html, open="r")
    lines <- readLines(cxn)

    # Create new file
    sink(path_md)

    # Write Jekyll header
    cat("---\n")
    cat("layout: page\n")
    cat(paste("title:", fn, "\n"))
    cat(paste("nav_order:", i, "\n"))
    cat(paste0("permalink: /", fn, "/\n"))
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
