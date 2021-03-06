% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{system2.5}
\alias{system2.5}
\title{Run a program asynchronously}
\usage{
system2.5(command, args = character(), input = NULL, env = character(),
  invisible = TRUE)
}
\description{
Like system2(wait=FALSE), but gives you a way to get the exit code.
}
\examples{

resultPromise <- system2.5("ls", c(Sys.getenv("HOME"), "baddir"))

resultPromise$then(
  onFulfilled = function(result) {
    cat(paste(readLines(result$stdout, warn = FALSE), collapse = "\\n"), "\\n")
    message(paste(readLines(result$stderr, warn = FALSE), collapse = "\\n"), "\\n")
    cat("\\nExited with status", result$exitstatus, "\\n")
  },
  onRejected = function(reason) {
    message("It failed :(")
  }
)

Sys.sleep(1)
shiny:::timerCallbacks$executeElapsed()
shiny:::flushReact()

}

