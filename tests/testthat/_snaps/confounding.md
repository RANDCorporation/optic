# confounding results have consistent structure

    Code
      list(dim = dim(linear_results), colnames = colnames(linear_results), nrow = nrow(
        linear_results), col_types = col_types)
    Output
      $dim
      [1] 240  48
      
      $colnames
       [1] "outcome"                "se_adjustment"          "estimate"              
       [4] "se"                     "variance"               "t_stat"                
       [7] "p_value"                "model_name"             "model_call"            
      [10] "model_formula"          "policy_speed"           "n_implementation_years"
      [13] "prior_control"          "bias_type"              "bias_size"             
      [16] "effect_direction"       "effect_magnitude"       "b0"                    
      [19] "b1"                     "b2"                     "b3"                    
      [22] "b4"                     "b5"                     "a1"                    
      [25] "a2"                     "a3"                     "a4"                    
      [28] "a5"                     "n"                      "mean_es_prior"         
      [31] "max_es_prior"           "mean_es_conf"           "max_es_conf"           
      [34] "mean_es_outcome"        "max_es_outcome"         "n_unique_enact_years"  
      [37] "mu1_prior"              "mu0_prior"              "sd_prior"              
      [40] "mu1_prior_old"          "mu0_prior_old"          "sd_prior_old"          
      [43] "mu1"                    "mu0"                    "sd"                    
      [46] "na.rm"                  "iter"                   "seed"                  
      
      $nrow
      [1] 240
      
      $col_types
                     outcome          se_adjustment               estimate 
                 "character"            "character"              "numeric" 
                          se               variance                 t_stat 
                   "numeric"              "numeric"              "numeric" 
                     p_value             model_name             model_call 
                   "numeric"            "character"            "character" 
               model_formula           policy_speed n_implementation_years 
                 "character"            "character"              "numeric" 
               prior_control              bias_type              bias_size 
                 "character"            "character"            "character" 
            effect_direction       effect_magnitude                     b0 
                 "character"              "numeric"              "numeric" 
                          b1                     b2                     b3 
                   "numeric"              "numeric"              "numeric" 
                          b4                     b5                     a1 
                   "numeric"              "numeric"              "numeric" 
                          a2                     a3                     a4 
                   "numeric"              "numeric"              "numeric" 
                          a5                      n          mean_es_prior 
                   "numeric"              "integer"              "numeric" 
                max_es_prior           mean_es_conf            max_es_conf 
                   "numeric"              "numeric"              "numeric" 
             mean_es_outcome         max_es_outcome   n_unique_enact_years 
                   "numeric"              "numeric"              "integer" 
                   mu1_prior              mu0_prior               sd_prior 
                   "numeric"              "numeric"              "numeric" 
               mu1_prior_old          mu0_prior_old           sd_prior_old 
                   "numeric"              "numeric"              "numeric" 
                         mu1                    mu0                     sd 
                   "numeric"              "numeric"              "numeric" 
                       na.rm                   iter                   seed 
                   "logical"              "integer"              "numeric" 
      

