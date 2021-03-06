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

  if(eed_is_eco(ecodyn)) {
    rets <- attr(ecodyn, "returns")
    argus <- attr(ecodyn, "arg_names")
    res <- rets[stringr::str_remove(rets, "^d") %in% argus]
  } else {
    res <- names(rlang::exec(ecodyn, !!!example_inputs))
  }

  res
}

get_dims <- function(example_inputs) {
  dims <- lapply(example_inputs, dim)
  ends <- cumsum(sapply(dims, prod))
  starts <- c(0, ends[-length(ends)]) + 1L
  names(starts) <- names(ends)
  list(dims = dims, inds = rbind(starts, ends))
}

