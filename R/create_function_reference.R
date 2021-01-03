
# This file will update the "function reference" section of the simba website (https://avi-kenny.github.io/simba/function-reference)
# !!!!! Run this file manually for now (eventually automate it via devtools)

library(rvest)
library(xml2)
library(tools)
library(htmltools)
library(magrittr)

fns <- c("add_constants", "add_creator", "add_method", "new_sim", "run",
         "run_on_cluster", "set_config", "set_levels", "set_script", "summary",
         "update", "update_on_cluster")

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

  # Extract HTML
  html <- read_html(path_html)
  contents <- html_nodes(html, "body>*")

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

  # Write HTML fragment
  for (line in contents) {

    line %<>% as.character()

    # Make replacements
    if (grepl("<pre>",line,fixed=T) || grepl("</pre>",line,fixed=T)) {
      line %<>% sub("<pre>", "```R", ., fixed=T)
      line %<>% sub("</pre>", "```", ., fixed=T)
      line %<>% gsub("&lt;", "<", ., fixed=T)
      line %<>% gsub("&gt;", ">", ., fixed=T)
    }
    line %<>% gsub("<code>", "<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>", ., fixed=T)
    line %<>% gsub("</code>", "</span>", ., fixed=T)

    # Write lines
    cat(line)
    cat("\n")

  }
  sink()

  # Delete original HTML file
  unlink(path_html)

}
