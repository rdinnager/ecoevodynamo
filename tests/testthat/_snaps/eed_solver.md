# extract_examples works as expected

    Code
      test1
    Output
      $ecodyn_fun
      function(Ns, other_dyn, traits, params, t, ...) {
        ## calculate distance between all pairs of species
        dists <- torch_cdist(traits, traits)
        competition <- torch_exp((-(dists^2) / (2 * params$comp^2)))
        dN_dt <- params$r * Ns * (1 - (torch_sum(Ns * competition, 2, keepdim = TRUE) / params$K))
      
        return(list(Ns = dN_dt))
      }
      <environment: package:ecoevodynamo>
      
      $example_inputs
      $example_inputs$Ns
      torch_tensor
        1
        2
        3
        4
        5
        6
        7
        8
        9
       10
       11
       12
       13
       14
       15
       16
       17
       18
       19
       20
      [ CPULongType{20,1} ]
      
      $example_inputs$traits
      torch_tensor
       1.0924 -0.3840  1.5466
      -0.8794  0.5825  0.2450
       0.6865  1.2499 -0.2265
       0.7868  0.2964  1.0306
       0.8020  0.0630 -0.4205
       0.2909 -0.1673 -0.6516
       0.4633  0.3805 -1.2693
      -0.4166 -1.2599  0.0877
       1.4063 -0.0380  0.5696
       0.3766 -1.3309 -0.2832
       1.2003 -0.5837  0.5559
      -0.0453 -0.2076  0.8973
      -0.5799  0.8089 -0.7445
       1.5060 -1.5342 -0.0166
       0.6552  1.2238 -1.5427
       0.9983 -1.0559  0.3593
       0.2398  0.7638 -0.5557
      -1.1532 -0.2305  0.9969
       0.5081  0.8369 -0.4880
       0.1047  1.2325 -0.4249
      [ CPUFloatType{20,3} ]
      
      

