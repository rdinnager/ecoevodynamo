#' Create an eed_eco object, containing a system of ODEs describing ecological dynamics
#'
#' @param expr An expression describing the ecological dynamics. The
#' expression can use any variable names you like, but `N`, `N_`, `X`, and `X_`
#' have special meaning. ``. The last statement in the expression must return a list,
#' where named components specify time derivatives of any variables by prefixing with
#' the letter 'd'. The named list must have at least one element named 'dN' or 'f',
#' use 'dN' to return the time derivative of 'N', the change in population sizes.
#' If you prefer you can return the 'f' instead, which is the per capita rate of
#' increase in population, which is what is used to measure fitness in `ecoevodynamo`.
#' Returning 'f' directly is often slightly more computationally efficient. Any other
#' variable mentioned in the expression can also have its time derivative returned, and
#' these variables will have their dynamics modeled in the system of ODEs as well, just
#' return an element with the same name as the variable but with prefixed with a 'd'. See
#' examples to get an idea how this works in practice. Lastly, any other named elements
#' besides 'f' or those that start with 'd' will be treated as global variables to be
#' passed to the next iteration of the model. Lastly there is one more special element that
#' can be passed in this list, and that is one named 'G'. This will be treated as a
#' dynamically calculated 'G' matrix which will be used to scale any selection gradient
#' calculations in a downstream `ecoevodynamo` model run (if used in an `ecodynamo` 'G'
#' will simply be ignored).
#'
#' @return An `eed_eco` object, which is essentially just a properly formatted function.
#' @export
#'
#' @examples
#' eed_eco({
#'   dists <- torch_cdist(X, X_, compute_mode = "use_mm_for_euclid_dist")
#'   competition <- torch_exp((-(dists^2) / (2 * comp^2)))
#'   weighted_pop <- torch_mm(competition, N_)
#'   dN_dt <- r * (1 - (weighted_pop / K))
#'
#'   list(f = dN_dt)
#' })
eed_eco <- function(expr) {
  expr <- rlang::enexpr(expr)
  ret_line <- expr[[length(expr)]]
  if(!rlang::is_call(ret_line)) {
    rlang::abort("The last line of input expression must be a call to `list`")
  }
  if(rlang::as_name(ret_line[[1]]) != "list") {
    rlang::abort("The last line of input expression must be a call to `list`")
  }
  returns <- rlang::call_args_names(expr[[length(expr)]])
  fun <- expr_to_fun(!!expr)
  attr(fun, "returns") <- returns
  class(fun) <- c("eed_eco", "function")
  fun
}

#' Print an `eed_eco` object prettily
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.eed_eco <- function(x, ...) {
  body_txt <- rlang::expr_deparse(rlang::fn_body(x))
  body_txt <- prettycode::highlight(body_txt)
  body_txt <- body_txt[c(-1, -length(body_txt))]
  cat(body_txt, sep = "\n")
}

#' Create an ecodynamo object for running an ecological dynamics ODE system.
#'
#' @param eco An expression (see [eed_eco()]), a function, or an [eed_eco()] object
#' specifying ecological dynamics as a set of ordinary differential equations.
#' See details for instructions on how to construct this argument.
#' @param example_inputs A named list where the names correspond to function arguments
#' of the function specified in `eco` and the elements are example inputs for those
#' arguments that have the dimensions of the desired system (these could be objects you
#' were using for testing the `eco` function, or vectors, matrices or arrays filled with
#' `NA`s, just make sure they have the correct dimensions, e.g. the first dimension will
#' generally have a size equal to the number of species you want to simulate for the
#' `N` and the `X` arguments -- see details). Make sure the `example_inputs`
#' actually do work because this is necessary to set up the model correctly. If
#' `example_inputs` is `NULL`, the example inputs will be inferred from the global
#' environment if possible. If you are unsure what inputs you [eed_eco()] object needs,
#' call [eed_inputs()] on it for a reminder.
#' @param jit Should the ecological dynamics be optimized using Just in Time (JIT) compilation?
#' This can speed up computations, but is currently somewhat experimental in `torch` so may
#' be unstable.
#' @param gpu Should the computations be run on a GPU? This again can speed up computations
#' for some kinds of models but adds additional overhead which will make other types of
#' models slower. Only does anything if you have a GPU version of `torch` installed (and you
#' have a GPU of course).
#'
#' @return An `ecodynamo` object which can be used to run an ecological dynamics simulation.
#' @export
#'
#' @examples
eed_ecodynamo <- function(eco, example_inputs, jit = FALSE, gpu = FALSE) {

}


