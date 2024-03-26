# Copyright (C) 2023, 2024 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
#
# This file is part of metaRange.
#
# metaRange is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.
#
# metaRange is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with metaRange. If not, see <http://www.gnu.org/licenses/>.

#' @title Process priority queue
#'
#' @description Creates a priority queue in form of an [R6][R6::R6Class] class,
#' that manages the correct process execution order.
#'
#' @return `<metaRangePriorityQueue>` A [metaRangePriorityQueue] object.
#' @export
metaRangePriorityQueue <- R6::R6Class("metaRangePriorityQueue",
    cloneable = FALSE,
    public = list(
        # ---------- initialization -----------
        #' @description Creates a new [metaRangePriorityQueue] object.
        #' Note: No reason to call this as user.
        #' The priority queue is created automatically when a simulation is created.
        #' @examples
        #' # Only for illustration purposes.
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr_queue
        #' @return `<metaRangePriorityQueue>` A [metaRangePriorityQueue] object.
        initialize = function() {
            private$queue <- vector("integer")
            private$future_queue <- vector("integer")
            private$processes <- new.env()
        },
        # ---------- public methods -----------
        #' @description Executes the next process in the queue.
        #' No reason to call this as user. The next process is executed
        #' automatically, when the previous process is finished.
        #' @param verbose `<logical>` Should timing and process information be
        #' printed when the process is executed?
        #' @examples
        #' # Only for illustration purposes.
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$update()
        #' pr_queue$execute_next_process(verbose = TRUE)
        #' @return `<logical>` `TRUE` if the next process has been executed,
        #' `FALSE` if not, in which case the queue is empty.
        execute_next_process = function(verbose) {
            if (self$is_empty()) {
                return(FALSE)
            }
            pr <- private$processes[[names(private$queue)[private$current_index]]]
            if (verbose) {
                start_time_pr <- Sys.time()
                message("|- ", pr$get_env_label(), " : ", pr$get_name())
            }

            pr$fun()
            private$current_index <- private$current_index + 1L
            if (private$current_index > length(private$queue)) {
                private$current_index <- 0L
            }
            if (verbose) {
                message("|---- ", format(Sys.time() - start_time_pr, digits = 2))
            }
            return(TRUE)
        },
        #' @description Add a process to the (future) queue.
        #' Users should only use this method if they added a process to the simulation
        #' via the add_process method of the simulation object with the argument
        #' `queue = FALSE`. Otherwise the process is added to the queue automatically.
        #' @param process `<metaRangeProcess>` A [metaRangeProcess] that should be added
        #' to the queue.
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
            private$processes[[process$get_PID()]] <- process
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
        #' Note: No reason to call this as user.
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
        #' Note: No reason to call this as user.
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
            return(length(private$queue) == 0L | private$current_index <= 0L)
        },
        #' @description Return the current queue.
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
        #' @description Return the future queue.
        #' @examples
        #' pr_queue <- metaRangePriorityQueue$new()
        #' pr <- metaRangeProcess$new("A", "1", \() {message("test")}, 1, new.env())
        #' pr_queue$enqueue(pr)
        #' pr_queue$get_future_queue()
        #' @return `<named int vector>` The future queue.
        get_future_queue = function() {
            return(private$future_queue)
        },
        #' @description Get the index of the process that will be executed next.
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
            remaining_names <- c()
            if (!self$is_empty()) {
                remaining_names <- names(tail(private$queue, n = -(private$current_index - 1L)))
            }
            cat("Remaining queue (this time step): ", length(remaining_names), "\n")
            utils::str(remaining_names, give.head = FALSE)
            cat("Future queue (next time step): ", length(private$future_queue), "\n")
            utils::str(names(private$future_queue), give.head = FALSE)
            return(invisible(self))
        }
    ),
    private = list(
        # ---------- private fields -------------
        # @field queue the order in which the processes should be executed.
        # Once this queue is created it is "immutable" in the sense that it
        # doesn't get updated until `self$update()` is called. This is done
        # to ensure that the order of the processes does not change during
        # one time step (sepcifically to mitigate the risk of accidental
        # infinite loops)
        queue = NULL,

        # @field future_queue the order in which the processes
        # should be executed in the next time step. This queue is where new processes are added to
        # or existing processes are removed.
        future_queue = NULL,

        # @field processes references to the processes that have been added to the queue.
        processes = NULL,

        # @field current_index
        current_index = 0L
    )
)
