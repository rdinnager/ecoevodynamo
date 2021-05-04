eed_get_history <- function(solver, as = c("tbl_df", "tensors", "list")) {
  as <- match.arg(as)

  if(is.null(solver$history)) {
    rlang::abort("solver has no history currently...")
  }

  hist <- apply(solver$history, 1,
                function(x) solver$vector_to_tensor_list(x[-1]))

  if(as == "tensors") {
    return(hist)
  }

  hist <- lapply(solver$history[ , 1],
                 function(x) convert_state(hist[[x]]) %>%
                   dplyr::mutate(time = x,
                                 id = paste0("species_",
                                             dplyr::row_number()))) %>%
    dplyr::bind_rows()

  hist


}

eed_get_state <- function(solver, as = c("tbl_df", "tensors", "list"),
                          species_only = as == "tbl_df") {
  as <- match.arg(as)

  state <- solver$current_state

  state <- convert_state(state, as = as, species_only = species_only)

  state

}

convert_state <- function(state, as = c("tbl_df", "tensors", "list"),
                          species_only = as == "tbl_df") {

  as <- match.arg(as)

  if(as == "tensors") {
    return(state)
  }

  state <- lapply(state, as.array)

  if(as == "tbl_df") {
    species_state <- tibble::as_tibble(
      as.data.frame(cbind(state$N, state$X)) %>%
        setNames(c("N", paste0("trait_", seq_len(ncol(state$X)))))
    )

    if(!species_only & any(!names(state) %in% c("N", "X"))) {
      state <- list(species = species_state,
                    other = states[!names(states) %in% c("N", "X")])
    } else {
      state <- species_state
    }
  }

  if(as == "list" & species_only) {
    state <- state[c("N", "X")]
  }

  state

}

eed_get_gradient <- function(solver, as = c("tbl_df", "tensors", "list"),
                             species_only = as == "tbl_df") {
  as <- match.arg(as)
  gradient <- solver$vector_to_tensor_list(
    solver$ode_fun(1,
                   solver$tensor_list_to_vector(solver$current_state),
                   solver$params,
                   efficiency = 1)[[1]]
  )

  gradient <- lapply(gradient, as.array)

  if(as == "tbl_df") {
    species_grad <- tibble::as_tibble(
      as.data.frame(cbind(gradient$Ns, gradient$traits)) %>%
        setNames(c("Ns", paste0("trait_", seq_len(ncol(gradient$traits)))))
    )

    if(!species_only & any(!names(gradient) %in% c("Ns", "traits"))) {
      state <- list(species = species_grad,
                    other = gradient[!names(gradient) %in% c("Ns", "traits")])
    } else {
      gradient <- species_grad
    }
  }

  if(as == "list" & species_only) {
    gradient <- gradient[c("Ns", "traits")]
  }

  gradient
}
