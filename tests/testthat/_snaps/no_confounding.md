# no_confounding results have consistent structure

    Code
      list(dim = dim(linear_results_df), colnames = colnames(linear_results_df),
      nrow = nrow(linear_results_df))
    Output
      $dim
      [1] 480  19
      
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
      [1] 480
      

# no_confounding summary results are consistent

    Code
      summary(linear_results_df)
    Output
         outcome          se_adjustment         estimate              se        
       Length:480         Length:480         Min.   :-13.7755   Min.   :0.6723  
       Class :character   Class :character   1st Qu.: -3.0920   1st Qu.:1.5417  
       Mode  :character   Mode  :character   Median : -0.1558   Median :1.9958  
                                             Mean   : -0.4327   Mean   :2.5318  
                                             3rd Qu.:  2.7744   3rd Qu.:2.8637  
                                             Max.   : 11.5096   Max.   :8.6299  
          variance          t_stat            p_value            mse        
       Min.   : 0.452   Min.   :-8.26189   Min.   :0.0000   Min.   : 8.814  
       1st Qu.: 2.377   1st Qu.:-1.26998   1st Qu.:0.0180   1st Qu.:22.505  
       Median : 3.983   Median :-0.09944   Median :0.1841   Median :22.751  
       Mean   : 9.148   Mean   : 0.02817   Mean   :0.3232   Mean   :20.435  
       3rd Qu.: 8.201   3rd Qu.: 1.44888   3rd Qu.:0.5839   3rd Qu.:22.855  
       Max.   :74.476   Max.   : 5.37481   Max.   :0.9680   Max.   :22.956  
        model_name         model_call        model_formula      policy_speed      
       Length:480         Length:480         Length:480         Length:480        
       Class :character   Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                  
                                                                                  
                                                                                  
       n_implementation_periods prior_control      effect_magnitude    n_units 
       Min.   : 3.0             Length:480         Min.   :0.000    Min.   :5  
       1st Qu.: 3.0             Class :character   1st Qu.:0.000    1st Qu.:5  
       Median : 6.5             Mode  :character   Median :1.986    Median :5  
       Mean   : 6.5                                Mean   :1.986    Mean   :5  
       3rd Qu.:10.0                                3rd Qu.:3.971    3rd Qu.:5  
       Max.   :10.0                                Max.   :3.971    Max.   :5  
       effect_direction        iter        seed     
       Length:480         Min.   :1   Min.   :9782  
       Class :character   1st Qu.:2   1st Qu.:9782  
       Mode  :character   Median :3   Median :9782  
                          Mean   :3   Mean   :9782  
                          3rd Qu.:4   3rd Qu.:9782  
                          Max.   :5   Max.   :9782  

