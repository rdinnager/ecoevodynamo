assert_ecodyn <- function(ecodyn) {
  args <- rlang::fn_fmls_names(ecodyn)
  if(!any(args == "Ns")) {
    rlang::abort("The provided ecodyn function must accept argument Ns")
  }
  if(!any(args == "traits")) {
    rlang::warn("The provided ecodyn function does not accept a traits argument. Resulting model will be just the ecological dynamics, with no evolutionary component (no evo in the ecoevo).")
  }
  invisible(NULL)
}

extract_dynamic <- function(ecodyn, example_inputs) {
  res <- rlang::exec(ecodyn, !!!example_inputs)
  names(res)
}
