
#' Create a ode solver for ecoevolutionary dynamics from an ode with only ecological dynamics
#'
#' @param ecodyn A call to the function specifying ecological dynamics with example inputs
#' as arguments (see details for how the function should be specified). Can also the function
#' itself, or the name of a function (of class character), in which case `example_inputs` must
#' not be `NULL`. The general form will be \code{ecodyn(Ns, [traits, t, G, ...])}
#' @param example_inputs A named list where the names correspond to function arguments
#' of the function specified in `ecodyn` and the elements are example inputs for those
#' arguments that have the dimensions of the desired system (these could be objects you
#' were using for testing the `ecodyn` function, or vectors, matrices or arrays filled with
#' `NA`s, just make sure they have the correct dimensions, e.g. the first dimension will
#' generally have a size equal to the number of species you want to simulate for th
#' `Ns` and the `traits` arguments -- see details). Make sure the \code{example_inputs}
#' actually do work because this is necessary to set up the model correctly.
#'
#' @return A set of functions that can be used to simulate the ecoevolutionary system. This
#' object is used by [eed_run()] to run the simulation.
#' @export
#' @importFrom zeallot %<-%
#' @importFrom stats setNames
#' @importFrom torch torch_tensor torch_ones_like autograd_grad
#'
#' @examples
#' mac_lev <- function(Ns, traits, params, t, ...) {
#'   ## calculate distance between all pairs of species
#'   dists <- torch_cdist(traits, traits)
#'   competition <- torch_exp((-(dists^2) / (2 * params$comp^2)))
#'   dN_dt <- params$r * Ns * (1 - (torch_sum(Ns * competition, 2, keepdim = TRUE) / params$K))
#'
#'   return(list(Ns = dN_dt))
#' }
#'
#' N_spec <- 20
#'
#' Ns <- torch_tensor(matrix(1:N_spec, nrow = N_spec, ncol = 1))
#' ## three traits
#' traits <- torch_randn(N_spec, 3)
#'
#' params <- list(comp = torch_scalar_tensor(0.5),
#'                K = torch_scalar_tensor(5),
#'                r = torch_scalar_tensor(1))
eed_solver <- function(ecodyn, example_inputs = NULL, jit = FALSE, gpu = FALSE) {

  ecodyn_quo <- rlang::enquo(ecodyn)

  c(ecodyn, example_inputs) %<-% extract_examples(ecodyn_quo, example_inputs)

  assert_ecodyn(ecodyn)

  dynamic <- extract_dynamic(ecodyn, example_inputs)
  using_f <-"df" %in% dynamic
  if(using_f) {
    dynamic[dynamic == "df"] <- "N"
  }

  param_names <- names(example_inputs)[!names(example_inputs) %in%
                                    c(dynamic, "t", "G", "X", "X_", "N_", "N")]

  params <- example_inputs[names(example_inputs) %in%
                             param_names]

  dynamic_inputs <- example_inputs[names(example_inputs) %in%
                                     c(dynamic, "X")]

  dims <- get_dims(dynamic_inputs)


  # vector_to_tensor_list <- function(y) {
  #   inputs <- setNames(lapply(seq_len(ncol(dims$inds)),
  #                             function(x)
  #                               torch::torch_tensor(y[dims$inds[1 , x]:dims$inds[2 , x]])$reshape(dims$dims[[x]])),
  #                      names(dims$dims))
  #   inputs
  # }

  vector_to_tensor_list <- function(y) {
    inputs <- setNames(lapply(seq_len(ncol(dims$inds)),
                              function(x) torch::torch_tensor(
                                array(y[dims$inds[1 , x]:dims$inds[2 , x]], dims$dims[[x]]))
                              ),
                       names(dims$dims))
    inputs
  }

  vector_to_R_list <- function(y) {
    inputs <- setNames(lapply(seq_len(ncol(dims$inds)),
                              function(x)
                                array(y[dims$inds[1 , x]:dims$inds[2 , x]], dims$dims[[x]])),
                       names(dims$dims))
    inputs
  }

  tensor_list_to_vector <- function(tensors) {
    dy <- lapply(tensors,
                 function(x) as.vector(as.array(x)))
    dy <- unlist(dy)
    dy
  }

  ecodyn_one <- function(inputs) {
    rlang::exec(ecodyn, !!!inputs)
  }

  if(jit) {
    ecodyn_one <- torch::jit_trace(ecodyn_one, example_inputs)
    input_names <- names(example_inputs)
  }

  ode_fun <- function(t, y, parms, efficiency, progress = NULL, last_t = NULL) {
    inputs <- vector_to_tensor_list(y)

    inputs <- c(inputs, parms)

    inputs$X_ <- inputs$X$clone()$detach()
    inputs$N_ <- inputs$N$clone()$detach()

    if(jit) {
      inputs <- inputs[input_names]
    }

    inputs$X$requires_grad <- TRUE

    if(gpu) {
      lapply(inputs, function(x) x$cuda())
    }

    #calc <- rlang::exec(ecodyn, !!!inputs)
    calc <- ecodyn_one(inputs)

    if(!using_f) {
      calc$N <- calc$N / inputs$N
    }

    names(calc) <- dynamic

    sel_grad <- torch::autograd_grad(calc$N,
                                     inputs$X,
                                     torch::torch_ones_like(calc$N))[[1]]

    sel_grad <- sel_grad * inputs$N * efficiency

    if(using_f) {
      calc$N <- calc$N * inputs$N
    }

    dy <- tensor_list_to_vector(c(calc, X = sel_grad))

    #.sim_state$current <- inputs

    if(!is.null(progress)) {
      done <- min(t / last_t, 0.9999)
      progress$update(done)
    }

    return(list(dy))

  }

  res <- list(vector_to_tensor_list = vector_to_tensor_list,
              vector_to_R_list = vector_to_R_list,
              tensor_list_to_vector = tensor_list_to_vector,
              ode_fun = ode_fun,
              ecodyn = ecodyn_one,
              params = params,
              dims = dims,
              example_inputs = example_inputs,
              current_state = dynamic_inputs,
              current_time = 0,
              history = NULL)

  res$current_gradient <- eed_get_gradient(res, "tensors")

  class(res) <- "eed_solver"
  res

}

extract_examples <- function(ecodyn_quo, example_inputs = NULL) {

  if(rlang::quo_is_call(ecodyn_quo)) {

    if(rlang::quo_get_expr(ecodyn_quo)[[1]] == "{") {
      ecodyn_fun <- eed_eco(rlang::quo_get_expr(ecodyn_quo))
    } else {

      ## generate example input from call arguments
      if(is.null(example_inputs)) {
        ecodyn_quo <- rlang::call_standardise(ecodyn_quo)
        example_inputs <- lapply(rlang::call_args(ecodyn_quo),
                                 rlang::eval_tidy,
                                 env = rlang::quo_get_env(ecodyn_quo))
        ecodyn_fun <- rlang::call_fn(ecodyn_quo)
      } else {
        ## example inputs provided so just use the call to find function
        ecodyn_fun <- rlang::call_fn(ecodyn_quo)
      }
    }
  } else {
    ecodyn_eval <- rlang::eval_tidy(ecodyn_quo)
    if(inherits(ecodyn_eval, "eed_eco")) {
      return()
    }
    if(rlang::is_function(ecodyn_eval)) {
      ecodyn_fun <- ecodyn_eval
    } else {
      if(rlang::is_character(ecodyn_eval)) {
        ecodyn_fun <- get(ecodyn_eval, rlang::caller_env())
      } else {
        rlang::abort("ecodyn argument must be a function call, a function, or a function name")
      }
    }
  }

  class(ecodyn_fun) <- "eed_eco"

  list(ecodyn_fun, example_inputs)
}



add_evo <- function(inputs) {
  ode_fun <- function(inputs) {
    #inputs$traits$requires_grad <- TRUE

    calc <- rlang::exec(ecodyn, !!!inputs)
    #calc <- do.call(ecodyn, inputs)

    # sel_grad <- torch::autograd_grad(calc[[1]],
    #                                  inputs[[2]],
    #                                  torch::torch_ones_like(calc[[1]]))
    #
    # dy <- c(calc, list(traits = sel_grad))
    # dy
    calc

  }

  if(jit) {
    ## jit code will go here once torch issue #529 is resolved
    ode_fun_jit <- torch::jit_trace(ode_fun, inputs)
  }

}


