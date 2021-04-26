# extract_examples works as expected

    Code
      test1
    Output
      [[1]]
      function(Ns, traits, params, t, ...) {
        ## calculate distance between all pairs of species
        dists <- torch_cdist(traits, traits)
        competition <- torch_exp((-(dists^2) / (2 * params$comp^2)))
        dN_dt <- params$r * Ns * (1 - (torch_sum(Ns * competition, 2, keepdim = TRUE) / params$K))
      
        return(list(Ns = dN_dt))
      }
      <environment: namespace:ecoevodynamo>
      
      [[2]]
      [[2]]$Ns
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
      
      [[2]]$traits
      torch_tensor
       0.4343  1.1486 -0.4919
      -0.8867  0.2287 -0.4446
       0.1848  0.9863  0.5712
      -2.1219 -0.1153  1.5263
       1.0991  0.9149  1.4063
      -1.3076  0.5313 -0.4491
      -1.0824  2.5830 -0.3784
      -0.6681 -0.4460 -0.4942
      -0.5336  0.9791  1.5577
      -0.3924  0.4283  0.6376
      -0.5494  0.7509 -0.8006
      -0.4935  0.5096  0.4226
      -0.6666  1.2957  1.3973
      -0.1674  0.4111  1.0710
       0.1041  0.8696 -1.3655
       0.0104  1.1654 -2.3204
       1.2601 -0.7946 -0.0214
       0.6222 -1.2178  1.4148
      -0.0926 -0.4765 -0.7357
       0.7932 -2.1862  0.7120
      [ CPUFloatType{20,3} ]
      
      

