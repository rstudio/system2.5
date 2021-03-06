# Violates: 2.2.4. onFulfilled or onRejected must not be called until the
# execution context stack contains only platform code. [3.1].

#' @import R6
#' @export
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

      private$doResolve(value)
      invisible()
    },
    reject = function(reason) {
      # Only allow this to be called once, then no-op.
      if (private$publicResolveRejectCalled)
        return(invisible())
      private$publicResolveRejectCalled <- TRUE

      private$doReject(reason)
      invisible()
    },
    then = function(onFulfilled = identity, onRejected = stop) {
      promise2 <- Promise$new()

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
    }
  )
)

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
