plot.eed_solver <- function(x, ...) {
  eed_plot_current_state(x, ...)
}

eed_plot_current_state <- function(solver, plot_gradient = TRUE, choose = c(1, 2),
                                   scale_grad = 1) {

  if(length(choose) > 2) {
    rlang::warn("choose should have length 2 or less. Only the first two element will be used.")
    choose <- choose[1:2]
  }

  state <- eed_get_state(solver) %>%
    dplyr::select("N", dplyr::all_of(paste0("trait_", choose)))
  colnames(state)[-1] <- c("trait_1", "trait_2")
  if(plot_gradient) {
    grad <- eed_get_gradient(solver, scale_grad = scale_grad) %>%
      dplyr::select(dplyr::all_of(paste0("trait_", choose)))
    colnames(grad) <- c("trait_1_grad", "trait_2_grad")
    state <- dplyr::bind_cols(state, grad) %>%
      dplyr::mutate("trait_1_vec" := .data$trait_1 + .data$trait_1_grad,
                    "trait_2_vec" := .data$trait_2 + .data$trait_2_grad)
  }

  p <- ggplot2::ggplot(state, ggplot2::aes_string("trait_1", "trait_2")) +
    ggplot2::geom_point(ggplot2::aes_string(size = "N")) +
    ggplot2::xlab(paste("Trait", choose[1])) +
    ggplot2::ylab(paste("Trait", choose[2])) +
    ggplot2::theme_minimal()

  if(plot_gradient) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(xend = "trait_1_vec",
                                                       yend = "trait_2_vec"),
                                   arrow = ggplot2::arrow(type = "closed",
                                                          length = ggplot2::unit(0.02, "npc")))
  }

  p

}

#' @importFrom gganimate animate
animate.eed_solver <- function(plot, ...) {
  eed_animate(plot, ...)
}

eed_animate <- function(solver, choose = c(1, 2), ...) {
  hist <- eed_get_history(solver)

  a <- ggplot2::ggplot(hist, ggplot2::aes_string("trait_1", "trait_2")) +
    ggplot2::geom_point(ggplot2::aes_string(group = "id", size = "N")) +
    ggplot2::coord_equal() +
    ggplot2::scale_size_area() +
    gganimate::transition_time(!!rlang::sym("time")) +
    ggplot2::theme_minimal()

  gganimate::animate(a, ...)

}
