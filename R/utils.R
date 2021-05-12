assert_ecodyn <- function(ecodyn) {
  args <- rlang::fn_fmls_names(ecodyn)
  if(!any(args == "N")) {
    rlang::abort("The provided ecodyn function must accept argument Ns")
  }
  if(!any(args == "X")) {
    rlang::warn("The provided ecodyn function does not accept a traits argument. Resulting model will be just the ecological dynamics, with no evolutionary component (no evo in the ecoevo).")
  }
  invisible(NULL)
}

extract_dynamic <- function(ecodyn, example_inputs) {
  res <- rlang::exec(ecodyn, !!!example_inputs)
  names(res)
}

eed_out <- function(...) {
  vars <- list(...)
  if(!any(names(vars) == "")) {
    return(vars)
  } else {
    globals <- vars[names(vars) == ""]
    c(vars[names(vars) != ""], globals = globals)
  }
}

#' @importFrom codetools findLocals
expr_to_fun <- function(expr) {

  expr2 <- rlang::enexpr(expr)
  vars <- all.vars(expr2)
  locals <- codetools::findLocals(expr2, rlang::base_env())
  arg_names <- union(setdiff(vars, locals), c("N", "N_", "X", "X_"))
  args <- replicate(length(arg_names), rlang::missing_arg())
  names(args) <- arg_names
  fun <- rlang::new_function(rlang::pairlist2(!!!args),
                             expr2)

  fun
}


