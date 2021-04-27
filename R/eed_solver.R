
#' Create a ode solver for ecoevolutionary dynamics from an ode with only ecological dynamics
#'
#' @param ecodyn A call to the function specifying ecological dynamics with example inputs
#' as arguments (see details for how the function should be specified). Can also the function
#' itself, or the name of a function (of class character), in which case `example_inputs` must
#' not be `NULL`.
#' @param example_inputs A named list where the names correspond to function arguments
#' of the function specified in `ecodyn` and the elements are example inputs for those
#' arguments that have the dimensions of the desired system (these could be objects you
#' were using for testing the `ecodyn` function, or vectors, matrices or arrays filled with
#' `NA`s, just make sure they have the correct dimensions, e.g. the first dimension will
#' generally have a size equal to the number of species you want to simulate for th
#' `Ns` and the `traits` arguments -- see details).
#' @param init_examples Should the \code{example_inputs} be used as default starting values
#' for future simulation runs?
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
eed_solver <- function(ecodyn, example_inputs = NULL, init_examples = TRUE) {

  ecodyn_quo <- rlang::enquo(ecodyn)

  c(ecodyn, example_inputs) %<-% extract_examples(ecodyn_quo, example_inputs)

  assert_ecodyn(ecodyn)

  example_inputs <- example_inputs[names(example_inputs) %in%
                                     c("Ns",
                                       "other_dyn",
                                       "traits",
                                       "G")]

  dims <- get_dims(example_inputs)


  vector_to_tensor_list <- function(y) {
    inputs <- setNames(lapply(seq_len(ncol(dims$inds)),
                              function(x)
                                torch::torch_tensor(y[dims$inds[1 , x]:dims$inds[2 , x]])$reshape(dims$dims[[x]])),
                       names(dims$dims))
    inputs
  }

  tensor_list_to_vector <- function(tensors) {
    dy <- lapply(tensors,
                 function(x) as.vector(as.array(x)))
    dy <- unlist(dy)
    dy
  }


  ode_fun <- function(t, y, parms, progress = NULL, last_t = NULL) {
    inputs <- vector_to_tensor_list(y)

    inputs <- c(inputs, list(params = parms))

    inputs$traits$requires_grad <- TRUE

    calc <- rlang::exec(ecodyn, !!!inputs)

    sel_grad <- torch::autograd_grad(calc$Ns,
                                     inputs$traits,
                                     torch::torch_ones_like(calc$Ns))

    dy <- tensor_list_to_vector(c(calc, setNames(sel_grad, "traits")))

    .sim_state$current <- inputs

    if(!is.null(progress)) {
      progress$update(t / last_t)
    }

    return(list(dy))

  }

  res <- list(vector_to_tensor_list = vector_to_tensor_list,
              tensor_list_to_vector = tensor_list_to_vector,
              ode_fun = ode_fun)

  if(init_examples) {
    res$init <- example_inputs
  }

  class(res) <- "eed_solver"
  res

}

extract_examples <- function(ecodyn_quo, example_inputs = NULL) {
  if(rlang::quo_is_call(ecodyn_quo)) {
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
  } else {
    ecodyn_eval <- rlang::eval_tidy(ecodyn_quo)
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

  list(ecodyn_fun, example_inputs)
}

get_dims <- function(example_inputs) {
  dims <- lapply(example_inputs, dim)
  ends <- cumsum(sapply(dims, prod))
  starts <- c(0, ends[-length(ends)]) + 1L
  names(starts) <- names(ends)
  list(dims = dims, inds = rbind(starts, ends))
}
