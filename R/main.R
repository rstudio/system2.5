#' Run a program asynchronously
#'
#' Like system2(wait=FALSE), but gives you a way to get the exit code.
#'
#' @examples
#'
#' resultPromise <- system2.5("ls", c(Sys.getenv("HOME"), "baddir"))
#'
#' resultPromise$then(
#'   onFulfilled = function(result) {
#'     cat(paste(readLines(result$stdout, warn = FALSE), collapse = "\n"), "\n")
#'     message(paste(readLines(result$stderr, warn = FALSE), collapse = "\n"), "\n")
#'     cat("\nExited with status", result$exitstatus, "\n")
#'   },
#'   onRejected = function(reason) {
#'     message("It failed :(")
#'   }
#' )
#'
#' Sys.sleep(1)
#' shiny:::timerCallbacks$executeElapsed()
#' shiny:::flushReact()
#'
#' @import shiny
#' @export
system2.5 <- function(command, args = character(), input = NULL,
  env = character(), invisible = TRUE) {

  outdir <- tempfile()
  if (!dir.create(outdir, mode = "0700")) {
    stop("Failed to create temp dir for job monitoring")
  }

  promise <- Promise$new()

  o <- observe({
    if (!file.exists(file.path(outdir, "exitcode"))) {
      invalidateLater(250, NULL)
    } else {
      o$destroy()
      if (nchar(outdir) < nchar(tempdir())) {
        stop("assertion failed")
      }

      exitstatus <- as.numeric(readLines(file.path(outdir, "exitcode")))
      stdout <- file(file.path(outdir, "job.stdout"), "rb")
      on.exit(close(stdout), add = TRUE)
      stderr <- file(file.path(outdir, "job.stderr"), "rb")
      on.exit(close(stderr), add = TRUE)

      on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

      result <- list(
        exitstatus = exitstatus,
        stdout = stdout,
        stderr = stderr
      )

      if (exitstatus == 0) {
        promise$resolve(result)
      } else {
        promise$reject(result)
      }
    }
  })

  system2(
    system.file("run.sh", package = "system2.5"),
    args = c(outdir, command, args),
    input = input,
    env = env,
    wait = FALSE
  )

  promise
}
