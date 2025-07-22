# concurrent results have consistent structure

    Code
      list(dim = dim(concurrent_results), colnames = colnames(concurrent_results),
      nrow = nrow(concurrent_results))
    Output
      $dim
      [1] 192  34
      
      $colnames
       [1] "se_adjustment"            "estimate1"               
       [3] "se1"                      "variance1"               
       [5] "test_stat1"               "p_value1"                
       [7] "estimate2"                "se2"                     
       [9] "variance2"                "test_stat2"              
      [11] "p_value2"                 "joint.eff"               
      [13] "joint.eff.se"             "variancej"               
      [15] "test_statj"               "joint.eff.pvalue"        
      [17] "effect_direction"         "effect_magnitude1"       
      [19] "effect_magnitude2"        "policy_speed"            
      [21] "rho"                      "years_apart"             
      [23] "ordered"                  "n_units"                 
      [25] "mean_distance"            "min_distance"            
      [27] "max_distance"             "n_implementation_periods"
      [29] "model_name"               "model_type"              
      [31] "model_call"               "model_formula"           
      [33] "iter"                     "seed"                    
      
      $nrow
      [1] 192
      

# concurrent summary results are consistent

    Code
      summary(concurrent_results)
    Output
       se_adjustment        estimate1            se1           variance1      
       Length:192         Min.   :-3.5261   Min.   :0.2997   Min.   :0.08984  
       Class :character   1st Qu.:-2.0143   1st Qu.:0.8166   1st Qu.:0.66686  
       Mode  :character   Median :-0.4783   Median :1.0244   Median :1.04946  
                          Mean   :-0.6642   Mean   :1.0511   Mean   :1.25322  
                          3rd Qu.: 0.4258   3rd Qu.:1.2932   3rd Qu.:1.67661  
                          Max.   : 2.2332   Max.   :1.8440   Max.   :3.40034  
         test_stat1         p_value1           estimate2           se2        
       Min.   :-3.8436   Min.   :0.0001299   Min.   :-3.965   Min.   :0.5408  
       1st Qu.:-1.8542   1st Qu.:0.0640814   1st Qu.:-1.926   1st Qu.:0.8478  
       Median :-0.4874   Median :0.3140060   Median :-1.182   Median :1.0956  
       Mean   :-0.6996   Mean   :0.3621759   Mean   :-1.254   Mean   :1.1087  
       3rd Qu.: 0.4376   3rd Qu.:0.6285400   3rd Qu.:-0.387   3rd Qu.:1.3515  
       Max.   : 1.5071   Max.   :0.9749450   Max.   : 1.166   Max.   :2.0985  
         variance2        test_stat2         p_value2           joint.eff      
       Min.   :0.2925   Min.   :-3.7063   Min.   :0.0002233   Min.   :-4.6319  
       1st Qu.:0.7188   1st Qu.:-1.8476   1st Qu.:0.0649954   1st Qu.:-2.6589  
       Median :1.2005   Median :-1.0964   Median :0.2433418   Median :-2.2229  
       Mean   :1.3386   Mean   :-1.1125   Mean   :0.3467433   Mean   :-1.9179  
       3rd Qu.:1.8267   3rd Qu.:-0.3642   3rd Qu.:0.5875785   3rd Qu.:-0.6968  
       Max.   :4.4037   Max.   : 1.4805   Max.   :0.9950285   Max.   : 1.0508  
        joint.eff.se      variancej        test_statj      joint.eff.pvalue   
       Min.   :0.3404   Min.   :0.1159   Min.   :-8.1716   Min.   :0.0000000  
       1st Qu.:0.5621   1st Qu.:0.3322   1st Qu.:-3.8540   1st Qu.:0.0001177  
       Median :0.6986   Median :0.4908   Median :-1.8394   Median :0.0607240  
       Mean   :0.9875   Mean   :1.3719   Mean   :-2.5577   Mean   :0.1800704  
       3rd Qu.:1.3554   3rd Qu.:1.8399   3rd Qu.:-0.9339   3rd Qu.:0.2868125  
       Max.   :2.5218   Max.   :6.3593   Max.   : 3.0814   Max.   :0.7917756  
       effect_direction   effect_magnitude1 effect_magnitude2 policy_speed      
       Length:192         Min.   :0.2275    Min.   :0.4551    Length:192        
       Class :character   1st Qu.:0.2275    1st Qu.:0.4551    Class :character  
       Mode  :character   Median :0.3413    Median :0.5689    Mode  :character  
                          Mean   :0.3413    Mean   :0.5689                      
                          3rd Qu.:0.4551    3rd Qu.:0.6826                      
                          Max.   :0.4551    Max.   :0.6826                      
            rho          years_apart ordered           n_units   mean_distance  
       Min.   :0.0000   Min.   :2    Mode:logical   Min.   :10   Min.   :636.0  
       1st Qu.:0.0000   1st Qu.:2    TRUE:192       1st Qu.:10   1st Qu.:742.4  
       Median :0.5000   Median :2                   Median :10   Median :777.8  
       Mean   :0.4667   Mean   :2                   Mean   :10   Mean   :767.0  
       3rd Qu.:0.9000   3rd Qu.:2                   3rd Qu.:10   3rd Qu.:797.4  
       Max.   :0.9000   Max.   :2                   Max.   :10   Max.   :870.4  
        min_distance    max_distance  n_implementation_periods  model_name       
       Min.   : 61.0   Min.   :1006   Min.   :3                Length:192        
       1st Qu.:153.0   1st Qu.:1068   1st Qu.:3                Class :character  
       Median :274.5   Median :1384   Median :3                Mode  :character  
       Mean   :299.3   Mean   :1335   Mean   :3                                  
       3rd Qu.:516.0   3rd Qu.:1461   3rd Qu.:3                                  
       Max.   :517.0   Max.   :1704   Max.   :3                                  
        model_type         model_call        model_formula           iter    
       Length:192         Length:192         Length:192         Min.   :1.0  
       Class :character   Class :character   Class :character   1st Qu.:1.0  
       Mode  :character   Mode  :character   Mode  :character   Median :1.5  
                                                                Mean   :1.5  
                                                                3rd Qu.:2.0  
                                                                Max.   :2.0  
            seed     
       Min.   :9782  
       1st Qu.:9782  
       Median :9782  
       Mean   :9782  
       3rd Qu.:9782  
       Max.   :9782  

