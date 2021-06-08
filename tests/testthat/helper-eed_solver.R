## specify a simple model to use in tests

set.seed(321)
torch_manual_seed(321)

mac_lev <- function(Ns, traits, comp, K, r) {
  ## calculate distance between all pairs of species
  dists <- torch_cdist(X, X_)
  competition <- torch_exp((-(dists^2) / (2 * comp^2)))
  weighted_pop <- torch_mm(competition, N_)
  df_dt <- r * (1 - (weighted_pop / K))
  list(f = df_dt)
}

N_spec <- 20

N_spec <- 20

Ns <- torch_rand(N_spec, 1L)
## three traits
traits <- torch_randn(N_spec, 3)
comp = torch_scalar_tensor(0.5)
K = torch_scalar_tensor(5)
r = torch_scalar_tensor(1)

