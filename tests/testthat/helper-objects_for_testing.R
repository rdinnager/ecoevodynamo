## specify a simple model to use in tests

mac_lev <- function(Ns, traits, params, t, ...) {
  ## calculate distance between all pairs of species
  dists <- torch_cdist(traits, traits)
  competition <- torch_exp((-(dists^2) / (2 * params$comp^2)))
  dN_dt <- params$r * Ns * (1 - (torch_sum(Ns * competition, 2, keepdim = TRUE) / params$K))

  return(list(Ns = dN_dt))
}

N_spec <- 20

Ns <- torch_tensor(matrix(1:N_spec, nrow = N_spec, ncol = 1))
## three traits
traits <- torch_randn(N_spec, 3)

params <- list(comp = torch_scalar_tensor(0.5),
               K = torch_scalar_tensor(5),
               r = torch_scalar_tensor(1))

