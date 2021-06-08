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

  if(is.null(example_inputs)) {
    rlang::warn("Attempting to find example_inputs in the calling environment. If this was not intended, please provide the example_inputs argument (a list)")
    argus <- rlang::fn_fmls_names(ecodyn_fun)
    names(argus) <- argus
    example_inputs <- purrr::map(argus,
                                 ~get(.x, rlang::caller_env()))
  }

  list(ecodyn_fun, example_inputs)
}


#' @importFrom codetools findLocals
expr_to_fun <- function(expr) {

  expr2 <- rlang::enexpr(expr)
  vars <- all.vars(expr2)
  locals <- codetools::findLocals(expr2, rlang::base_env())
  vars <- setdiff(vars, locals)
  arg_names <- union(vars, c("N", "N_", "X", "X_"))
  args <- replicate(length(arg_names), rlang::missing_arg())
  names(args) <- arg_names
  fun <- rlang::new_function(rlang::pairlist2(!!!args),
                             expr2)

  attr(fun, "vars") <- vars
  attr(fun, "arg_names") <- arg_names

  fun
}
