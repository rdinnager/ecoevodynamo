
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
#'
#' @return A set of functions that can be used to simulate the ecoevolutionary system. This
#' object is used by [eed_run()] to run the simulation.
#' @export
#' @importFrom zeallot %<-%
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
eed_solver <- function(ecodyn, example_inputs = NULL) {

  ecodyn_quo <- rlang::enquo(ecodyn)

  c(ecodyn, example_inputs) %<-% extract_examples(ecodyn_quo, example_inputs)

  list(ecodyn_fun = ecodyn, example_inputs = example_inputs)

}

extract_examples <- function(ecodyn_quo, example_inputs) {
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
        ecodyn_fun <- get(ecodyn_eval)
      } else {
        rlang::abort("ecodyn argument must be a function call, a function, or a function name")
      }
    }
  }

  list(ecodyn_fun, example_inputs)
}
