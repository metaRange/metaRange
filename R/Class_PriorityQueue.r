# Author: Stefan Fallert
# Date: 18.06.2023
# License: GPL-3 (See License.md)
#
#' @title Process priority queue
#'
#' @description Creates a priority queue in form of an [R6][R6::R6Class] class,
#' that manages the correct process execution order.
#'
#' @return A `<[metaRangePriorityQueue]>` object
#' @export
metaRangePriorityQueue <- R6::R6Class("metaRangePriorityQueue",
    cloneable = FALSE,
    public = list(
        # ---------- initialization -----------
        #' @description Creates a new [metaRangePriorityQueue] object
        #' Note: Only for illustration purposes. No reason to call this as user.
        #' The priority queue is created automatically when a simulation is created.
        #' @examples
        #' # Only for illustration purposes.
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr_queue
        initialize = function() {
            private$queue <- vector("integer")
            private$future_queue <- vector("integer")
            private$procecces <- new.env()
        },
        # ---------- public methods -----------
        #' @description Executes the next process in the queue.
        #' No reason to call this as user. The next process is executed automatically.
        #' @param verbose `<logical>` Print timing and information or not.
        #' @examples
        #' # Only for illustration purposes.
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$update()
        #' pr_queue$execute_next_process(verbose = TRUE)
        #' @return `<logical>` `TRUE` if the next process has been executed,
        #' `FALSE` if not and the queue is empty.
        execute_next_process = function(verbose) {
            if (self$is_empty()) {
                return(FALSE)
            }
            pr <- private$procecces[[names(private$queue)[private$current_index]]]
            if (verbose) {
                start_time_pr <- Sys.time()
                message("|- ", pr$get_env_label(), " : ", pr$get_name())
            }

            pr$fun()
            private$current_index <- private$current_index + 1L
            if (verbose) {
                message("|---- ", format(Sys.time() - start_time_pr, digits = 2))
            }
            return(TRUE)
        },
        #' @description Add a process to the (future) queue.
        #' Users should only use this method if they added a process to the simulation
        #' via the add_process method of the simulation object with the argument
        #' `queue = FALSE`. Otherwise the process is added to the queue automatically.
        #' @param process `<[metaRangeProcess]>` a process that should be added to the queue.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$get_future_queue()
        #' @return `<boolean>` `TRUE` on success `FALSE` on failure.
        enqueue = function(process) {
            if (!checkmate::test_class(process, "metaRangeProcess")) {
                warning("failed to queue process. Argument 'PID' must be a string")
                return(invisible(FALSE))
            }
            private$procecces[[process$get_PID()]] <- process
            new_item <- process$get_priority()
            names(new_item) <- process$get_PID()
            private$future_queue <- c(private$future_queue, new_item)
            self$sort_future_queue()
            return(invisible(TRUE))
        },
        #' @description Remove a process from the (future) queue.
        #' Useful to remove a process from the queue if it is no longer needed.
        #' E.g. if a species went extinct.
        #' @param PID `<string>` the ID of the process, that should be dequeued.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$dequeue(pr$get_PID())
        #' pr_queue$get_future_queue()
        #' @return `<boolean>` `TRUE` on success `FALSE` on failure.
        dequeue = function(PID = NULL) {
            if (!checkmate::test_string(PID)) {
                warning("failed to dequeue process. Argument 'PID' must be a string")
                return(invisible(FALSE))
            }
            if (!PID %in% names(private$future_queue)) {
                warning("failed to dequeue process. ", PID, " was not found.")
                return(invisible(FALSE))
            }
            private$future_queue <- private$future_queue[!names(private$future_queue) %in% PID]
            self$sort_future_queue()
            return(invisible(TRUE))
        },
        #' @description Sort the (future) queue based on the execution priority.
        #' This method is called automatically when a process is added to the queue.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$sort_future_queue()
        #' # at least no error
        #' @return `<invisible self>`.
        sort_future_queue = function() {
            private$future_queue <- sort(private$future_queue)
            return(invisible(self))
        },
        #' @description Update and reset the queue.
        #' This method is called automatically at the end of each time step.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$update()
        #' pr_queue$get_queue()
        #' @return `<invisible self>`.
        update = function() {
            self$sort_future_queue()
            private$queue <- private$future_queue
            private$current_index <- 1L
            return(invisible(self))
        },
        #' @description Check if the queue is empty.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' stopifnot(pr_queue$is_empty())
        #' @return `<boolean>` `TRUE` if queue is empty `FALSE` otherwise.
        is_empty = function() {
            return(length(private$queue) == 0L | private$current_index > length(private$queue))
        },
        #' @description Get the current queue.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$update()
        #' pr_queue$get_queue()
        #' @return `<named int vector>` The current queue.
        get_queue = function() {
            return(private$queue)
        },
        #' @description Get the future queue.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$get_future_queue()
        #' @return `<named int vector>` The future queue.
        get_future_queue = function() {
            return(private$future_queue)
        },
        #' @description Get the number / index of the next to be executed process.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$update()
        #' pr_queue$get_current_index()
        #' @return `<integer>` The index.
        get_current_index = function() {
            return(private$current_index)
        },

        #' @description Prints information about the queue to the console.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr_queue$print()
        #' @return `<invisible self>`.
        print = function() {
            cat("At process: ", private$current_index, "out of: ", length(private$queue), "\n")
            cat("Remaining queue: \n")
            if (self$is_empty()) {
                cat("--- empty", "\n")
            } else {
                show(tail(private$queue, n = -(private$current_index - 1L)))
            }
            cat("Future (next time step) queue: \n")
            if (length(private$future_queue) == 0) {
                cat("--- empty", "\n")
            } else {
                show(private$future_queue)
            }
            return(invisible(self))
        }
    ),
    private = list(
        # ---------- private fields -------------
        # @field queue the order in which the processes should be executed.
        # Once this queue is created it is "immutable" in the sense that it doesn't get updated until
        # `self$update()` is called. This is done to ensure that the order of the processes
        # doesn't change during one time step.
        queue = NULL,

        # @field future_queue the order in which the processes
        # should be executed in the next time step. This queue is where new processes are added to
        # or existing processes are removed.
        future_queue = NULL,

        # @field procecces references to the processes that have been added to the queue.
        procecces = NULL,

        # @field current_index
        current_index = 0L
    )
)
