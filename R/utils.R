assert_ecodyn <- function(ecodyn) {
  args <- rlang::fn_fmls_names(ecodyn)
  if(!any(args == "Ns")) {
    rlang::abort("The provided ecodyn function must accept argument Ns")
  }
  if(!any(args == "traits")) {
    rlang::warn("The provided ecodyn function does not accept a traits argument. Resulting model will be just the ecological dynamics, with no evolutionary component (no evo in the ecoevo).")
  }
  if(any(!args %in% c("Ns",
                      "other_dyn",
                      "traits",
                      "G",
                      "params",
                      "t"))) {
    rlang::warn("The provided ecodyn function has non-standard arguments (not Ns, other_dyn, traits, G, params, or t). They will be assumed to be non-dynamic. It might be better to use params for these arguments.. If you want dynamic variables, use other_dyn, which can be a list")
  }
  invisible(NULL)
}
