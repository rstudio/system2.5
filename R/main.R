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
      # TODO: These close()es will have to change to maybe finalizer based
      # once promise callbacks are called asynchronously
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

#' @export
forkinvoke <- function(expr, ...) {
  info <- list(
    func = shiny::exprToFunction(expr),
    options = options(),
    search = search(),
    args = list(...)
  )

  rdspath <- tempfile(fileext = ".rds")
  saveRDS(info, rdspath, compress = FALSE)

  p <- system2.5(
    "R", # TODO: Make this the current R
    c(
      "--slave",
      "--no-restore",
      "-e",
      # TODO: better escape
      paste0("system2.5:::doinvoke\\(\\'", rdspath, "\\'\\)")
    )
  )

  p$then(
    onFulfilled = function(result) {
      on.exit(unlink(rdspath))

      if (result$exitstatus) {
        # Command failed
        stop("Command failed, ",
          paste(readLines(result$stderr, warn=FALSE), collapse="\n")
        )
      }

      readRDS(rdspath)
    },
    onRejected = function(result) {
      msg <- paste(readLines(result$stderr, warn=FALSE), collapse="\n")
      stop(msg)
    }
  )
}

doinvoke <- function(path) {
  info <- readRDS(path)

  result <- info$func()

  saveRDS(result, path, compress = FALSE)
}
