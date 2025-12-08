# no_confounding basic test (CRAN check)

    Code
      list(test_type = "simple_cran", dim = dim(simple_results), colnames = colnames(
        simple_results), nrow = nrow(simple_results), col_types = col_types)
    Output
      $test_type
      [1] "simple_cran"
      
      $dim
      [1] 10 19
      
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
      [1] 10
      
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
      

# no_confounding comprehensive test with all models

    Code
      list(test_type = "comprehensive_full_suite", dim = dim(comprehensive_results),
      colnames = colnames(comprehensive_results), nrow = nrow(comprehensive_results),
      col_types = col_types)
    Output
      $test_type
      [1] "comprehensive_full_suite"
      
      $dim
      [1] 242  19
      
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
      [1] 242
      
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
      

