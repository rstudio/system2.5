#' @export
#' @import R6
Promise <- R6::R6Class("Promise",
  private = list(
    state = "pending",
    value = NULL,
    publicResolveRejectCalled = FALSE,
    onFulfilled = list(),
    onRejected = list(),
    onFinally = list(),
    rejectionHandled = FALSE,

    # Private resolve/reject differs from public resolve/reject
    # in that the private versions are allowed to be called
    # more than once, whereas public ones no-op after the first
    # time they are invoked.
    doResolve = function(value) {
      if (inherits(value, "Promise")) {
        value$then(
          private$doResolve,
          private$doReject
        )
      } else {
        private$doResolveFinalValue(value)
      }
    },
    doReject = function(reason) {
      if (inherits(reason, "Promise")) {
        reason$then(
          private$doResolve,
          private$doReject
        )
      } else {
        private$doRejectFinalReason(reason)
      }
    },
    doResolveFinalValue = function(value) {
      private$value <- value
      private$state <- "fulfilled"

      later::later(function() {
        lapply(private$onFulfilled, function(f) {
          f(private$value)
        })
        private$onFulfilled <- list()
      })
    },
    doRejectFinalReason = function(reason) {
      private$value <- reason
      private$state <- "rejected"

      later::later(function() {
        lapply(private$onRejected, function(f) {
          private$rejectionHandled <- TRUE
          f(private$value)
        })
        private$onRejected <- list()

        if (!private$rejectionHandled) {
          reg.finalizer(self, function(obj) {
            if (!private$rejectionHandled) {
              warning("Unhandled promise error: ", reason$message, call. = FALSE)
              shiny::printError(reason)
            }
          }, onexit = TRUE)
        }
      })
    }
  ),
  public = list(
    status = function() {
      private$state
    },
    resolve = function(value) {
      # Only allow this to be called once, then no-op.
      if (private$publicResolveRejectCalled)
        return(invisible())
      private$publicResolveRejectCalled <- TRUE

      tryCatch(
        {
          force(value)
          private$doResolve(value)
        },
        error = function(err) {
          private$doReject(err)
        }
      )

      invisible(self)
    },
    reject = function(reason) {
      # Only allow this to be called once, then no-op.
      if (private$publicResolveRejectCalled)
        return(invisible())
      private$publicResolveRejectCalled <- TRUE

      private$doReject(reason)
      invisible(self)
    },
    then = function(onFulfilled = identity, onRejected = stop) {
      promise2 <- new_promise(function(resolve, reject) {

        res <- promiseDomain$onThen(onFulfilled, onRejected)
        if (!is.null(res)) {
          onFulfilled <- res$onFulfilled
          onRejected <- res$onRejected
        }

        handleIt <- function(func, value) {
          tryCatch(
            {
              val <- func(value)
              resolve(val)
            },
            error = function(e) {
              reject(e)
            }
          )
        }

        if (private$state == "pending") {
          if (is.function(onFulfilled)) {
            private$onFulfilled <- c(private$onFulfilled, list(
              function(value) {
                handleIt(onFulfilled, value)
              }
            ))
          }
          if (is.function(onRejected)) {
            private$onRejected <- c(private$onRejected, list(
              function(reason) {
                handleIt(onRejected, reason)
              }
            ))
          }
        } else if (private$state == "fulfilled") {
          later::later(~handleIt(onFulfilled, private$value))
        } else if (private$state == "rejected") {
          later::later(function() {
            private$rejectionHandled <- TRUE
            handleIt(onRejected, private$value)
          })
        } else {
          stop("Unexpected state ", private$state)
        }
      })

      invisible(promise2)
    },
    catch = function(onRejected) {
      invisible(self$then(onRejected = onRejected))
    },
    finally = function(onFinally) {
      invisible(self$then(
        onFulfilled = function(value) {
          onFinally()
          value
        },
        onRejected = function(reason) {
          onFinally()
          stop(reason)
        }
      ))
    }
  )
)

#' @export
new_promise <- function(actionFunc) {
  p <- Promise$new()

  tryCatch(
    actionFunc(p$resolve, p$reject),
    error = function(e) {
      if (p$status() == "pending") {
        p$reject(e)
      } else {
        # Too late to do anything useful. Just notify.
        warning(e)
      }
    }
  )
  structure(
    list(
      then = p$then,
      catch = p$catch,
      finally = p$finally
    ),
    class = "promise"
  )
}

#' @export
resolved <- function(value) {
  new_promise(function(resolve, reject) {
    resolve(value)
  })
}

#' @export
then <- function(promise, onFulfilled = NULL, onRejected = NULL) {
  promise$then(onFulfilled = onFulfilled, onRejected = onRejected)
}

#' @export
catch <- function(promise, onRejected) {
  promise$catch(onRejected)
}

#' @export
finally <- function(promise, onFinally) {
  promise$finally(onFinally)
}

promiseDomain <- list(
  onThen = function(onFulfilled, onRejected) {
    domain <- current_promise_domain()

    if (is.null(domain))
      return()
    if (is.null(onFulfilled) && is.null(onRejected))
      return()

    results <- list()
    if (!is.null(onFulfilled)) {
      newOnFulfilled <- domain$wrapOnFulfilled(onFulfilled)
      results$onFulfilled <- function(value) {
        with_promise_domain(domain, newOnFulfilled(value))
      }
    }
    if (!is.null(onRejected)) {
      newOnRejected <- domain$wrapOnRejected(onRejected)
      results$onRejected <- function(reason) {
        with_promise_domain(domain, newOnRejected(reason))
      }
    }
    results
  }
)

globals <- new.env(parent = emptyenv())

current_promise_domain <- function() {
  globals$domain
}

#' @export
with_promise_domain <- function(domain, expr, replace = FALSE) {
  oldval <- current_promise_domain()
  if (replace)
    globals$domain <- domain
  else
    globals$domain <- compose_domains(oldval, domain)
  on.exit(globals$domain <- oldval)

  force(expr)
}

#' @export
new_promise_domain <- function(
  wrapOnFulfilled = identity,
  wrapOnRejected = identity,
  ...
) {
  list2env(list(
    wrapOnFulfilled = wrapOnFulfilled,
    wrapOnRejected = wrapOnRejected,
    ...
  ), parent = emptyenv())
}


compose_domains <- function(base, new) {
  if (is.null(base)) {
    return(new)
  }

  list(
    wrapOnFulfilled = function(onFulfilled) {
      new$wrapOnFulfilled(
        base$wrapOnFulfilled(onFulfilled)
      )
    },
    wrapOnRejected = function(onRejected) {
      new$wrapOnRejected(
        base$wrapOnRejected(onRejected)
      )
    }
  )
}

#' @export
fmap.promise <- function(.m, .f, ...) {
  .m$then(function(val) {
    .f(val, ...)
  })
}

# p <- Promise$new()
# p$then(function(val) { print(val) })
# p$resolve(10)
# p$then(function(val) { str(val) })
# p$then()$then(function(val) { print(summary(val)) })
#
# p <- Promise$new()
# p$then(function(val) { print("passed")},
#   function(reason) { "It failed" })$then(
#     function(val) { print(val) }
#   )
# p$reject(simpleError("boom"))
