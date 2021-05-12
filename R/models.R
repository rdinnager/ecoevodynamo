eed_eco_maclevins <- function(N, N_, X, X_, comp, K, r) {
  ## calculate distance between all pairs of species
  dists <- torch_cdist(X, X_, compute_mode = "use_mm_for_euclid_dist")
  competition <- torch_exp((-(dists^2) / (2 * comp^2)))
  weighted_pop <- torch_mm(competition, N_)
  dN_dt <- r * (1 - (weighted_pop / K))

  return(list(f = dN_dt))
}
