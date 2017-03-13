# Violates: 2.2.4. onFulfilled or onRejected must not be called until the
# execution context stack contains only platform code. [3.1].

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

      lapply(private$onFulfilled, function(f) {
        f(private$value)
      })
      private$onFulfilled <- list()
    },
    doRejectFinalReason = function(reason) {
      private$value <- reason
      private$state <- "rejected"

      lapply(private$onRejected, function(f) {
        f(private$value)
      })
      private$onRejected <- list()
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
      promise2 <- Promise$new()

      res <- promiseDomain$onThen(onFulfilled, onRejected)
      if (!is.null(res)) {
        onFulfilled <- res$onFulfilled
        onRejected <- res$onRejected
      }

      handleIt <- function(func, value) {
        tryCatch(
          {
            val <- func(value)
            promise2$resolve(val)
          },
          error = function(e) {
            promise2$reject(e)
          }
        )
      }

      if (private$state == "pending") {
        private$onFulfilled <- c(private$onFulfilled, list(
          function(value) {
            handleIt(onFulfilled, value)
          }
        ))
        private$onRejected <- c(private$onRejected, list(
          function(reason) {
            handleIt(onRejected, reason)
          }
        ))
      } else if (private$state == "fulfilled") {
        handleIt(onFulfilled, private$value)
      } else if (private$state == "rejected") {
        handleIt(onRejected, private$value)
      } else {
        stop("Unexpected state ", private$state)
      }

      invisible(promise2)
    },
    catch = function(onRejected) {
      self$then(onRejected = onRejected)
    },
    finally = function(onFinally) {
      self$then(
        onFulfilled = function(value) {
          onFinally()
          value
        },
        onRejected = function(reason) {
          onFinally()
          stop(reason)
        }
      )
    }
  )
)

PromiseDomain <- R6::R6Class("PromiseDomain",
  private = list(
  ),
  public = list(
    onThen = function(onFulfilled, onRejected) {
      NULL
    }
  )
)

#' @include stack.R
promiseDomains <- Stack$new()
promiseDomain <- list(
  onThen = function(onFulfilled, onRejected) {
    if (promiseDomains$size() == 0) {
      return()
    } else if (promiseDomains$size() == 1) {
      return(promiseDomains$peek()$onThen(onFulfilled, onRejected))
    } else {
      state <- list(onFulfilled = onFulfilled, onRejected = onRejected)
      for (pd in promiseDomains$as_list())
        state <- pd$onThen(state$onFulfilled, state$onRejected)
      state
    }
  }
)

#' @export
withPromiseDomain <- function(domain, expr) {
  promiseDomains$push(domain)
  on.exit(promiseDomains$pop())

  force(expr)
}

#' @export
fmap.Promise <- function(.m, .f, ...) {
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
