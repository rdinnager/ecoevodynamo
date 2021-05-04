eed_run <- function(solver, times, init_dynamic = NULL, params = NULL,
                    efficiency = 0.01,
                    append_history = TRUE, verbose = TRUE, ...) {

  if(!inherits(solver, "eed_solver")) {

  }

  if(is.null(init_dynamic)) {
    init_dynamic <- solver$current_state
  }

  if(is.null(params)) {
    params <- solver$params
  }


  init_y <- solver$tensor_list_to_vector(init_dynamic)

  if(verbose) {
    pb <- progress::progress_bar$new(total = 100)
  } else {
    pb <- NULL
  }

  states <- deSolve::ode(init_y,
                         times,
                         solver$ode_fun,
                         params,
                         efficiency = efficiency,
                         progress = pb,
                         last_t = max(times),
                         ...)

  pb$update(1.0)

  new_state <- solver$vector_to_tensor_list(states[nrow(states), -1])

  if(append_history) {
    solver$history <- rbind(solver$history, states)
  } else {
    solver$history <- states
  }

  solver$current_state <- new_state

  solver$current_time <- solver$current_time + max(times)

  solver

}
