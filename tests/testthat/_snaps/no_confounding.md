# no_confounding results have consistent structure

    Code
      list(dim = dim(linear_results), colnames = colnames(linear_results), nrow = nrow(
        linear_results), col_types = col_types)
    Output
      $dim
      [1] 1056   19
      
      $colnames
       [1] "outcome"                  "se_adjustment"           
       [3] "estimate"                 "se"                      
       [5] "variance"                 "t_stat"                  
       [7] "p_value"                  "mse"                     
       [9] "model_name"               "model_call"              
      [11] "model_formula"            "policy_speed"            
      [13] "n_implementation_periods" "prior_control"           
      [15] "effect_magnitude"         "n_units"                 
      [17] "effect_direction"         "iter"                    
      [19] "seed"                    
      
      $nrow
      [1] 1056
      
      $col_types
                       outcome            se_adjustment                 estimate 
                   "character"              "character"                "numeric" 
                            se                 variance                   t_stat 
                     "numeric"                "numeric"                "numeric" 
                       p_value                      mse               model_name 
                     "numeric"                "numeric"              "character" 
                    model_call            model_formula             policy_speed 
                   "character"              "character"              "character" 
      n_implementation_periods            prior_control         effect_magnitude 
                     "numeric"              "character"                "numeric" 
                       n_units         effect_direction                     iter 
                     "numeric"              "character"                "integer" 
                          seed 
                     "numeric" 
      

