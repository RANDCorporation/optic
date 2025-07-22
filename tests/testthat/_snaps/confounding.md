# confounding results have consistent structure

    Code
      list(dim = dim(linear_results_df), colnames = colnames(linear_results_df),
      nrow = nrow(linear_results_df))
    Output
      $dim
      [1] 180  48
      
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
      [1] 180
      

# confounding summary results are consistent

    Code
      summary(linear_results_df)
    Output
         outcome          se_adjustment         estimate             se        
       Length:180         Length:180         Min.   :-4.1437   Min.   :0.4051  
       Class :character   Class :character   1st Qu.:-0.3988   1st Qu.:0.5150  
       Mode  :character   Mode  :character   Median : 0.6127   Median :0.7012  
                                             Mean   : 0.8062   Mean   :0.9220  
                                             3rd Qu.: 2.0733   3rd Qu.:1.2300  
                                             Max.   : 6.2564   Max.   :2.4476  
          variance          t_stat           p_value          model_name       
       Min.   :0.1641   Min.   :-6.2199   Min.   :0.000000   Length:180        
       1st Qu.:0.2652   1st Qu.:-0.7357   1st Qu.:0.007475   Class :character  
       Median :0.4917   Median : 0.6756   Median :0.176884   Mode  :character  
       Mean   :1.1069   Mean   : 0.9208   Mean   :0.279874                     
       3rd Qu.:1.5140   3rd Qu.: 2.0307   3rd Qu.:0.486573                     
       Max.   :5.9907   Max.   :10.6900   Max.   :0.998229                     
        model_call        model_formula      policy_speed      
       Length:180         Length:180         Length:180        
       Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character  
                                                               
                                                               
                                                               
       n_implementation_years prior_control       bias_type        
       Min.   :0              Length:180         Length:180        
       1st Qu.:0              Class :character   Class :character  
       Median :0              Mode  :character   Mode  :character  
       Mean   :0                                                   
       3rd Qu.:0                                                   
       Max.   :0                                                   
        bias_size         effect_direction   effect_magnitude       b0        
       Length:180         Length:180         Min.   :0        Min.   :-5.100  
       Class :character   Class :character   1st Qu.:0        1st Qu.:-4.550  
       Mode  :character   Mode  :character   Median :0        Median :-4.250  
                                             Mean   :0        Mean   :-4.275  
                                             3rd Qu.:0        3rd Qu.:-3.875  
                                             Max.   :0        Max.   :-3.600  
             b1               b2                b3                 b4          
       Min.   :0.0500   Min.   :0.00100   Min.   :0.000000   Min.   :0.000000  
       1st Qu.:0.0500   1st Qu.:0.04500   1st Qu.:0.000000   1st Qu.:0.000000  
       Median :0.0550   Median :0.05000   Median :0.000150   Median :0.000150  
       Mean   :0.1175   Mean   :0.07175   Mean   :0.004733   Mean   :0.004733  
       3rd Qu.:0.1525   3rd Qu.:0.07750   3rd Qu.:0.005125   3rd Qu.:0.006125  
       Max.   :0.3700   Max.   :0.22000   Max.   :0.025000   Max.   :0.025000  
             b5                  a1               a2              a3        
       Min.   :0.0000000   Min.   :0.0100   Min.   :0.010   Min.   :0.0000  
       1st Qu.:0.0000000   1st Qu.:0.0775   1st Qu.:0.040   1st Qu.:0.0000  
       Median :0.0000015   Median :0.1500   Median :0.050   Median :0.0050  
       Mean   :0.0037573   Mean   :0.2025   Mean   :0.055   Mean   :0.0275  
       3rd Qu.:0.0012913   3rd Qu.:0.2750   3rd Qu.:0.065   3rd Qu.:0.0325  
       Max.   :0.0250000   Max.   :0.5000   Max.   :0.110   Max.   :0.1000  
             a4              a5                n         mean_es_prior     
       Min.   :0.000   Min.   :0.00000   Min.   :20.00   Min.   :-0.21182  
       1st Qu.:0.000   1st Qu.:0.00000   1st Qu.:24.75   1st Qu.: 0.08587  
       Median :0.005   Median :0.00050   Median :29.00   Median : 0.26035  
       Mean   :0.005   Mean   :0.00275   Mean   :29.97   Mean   : 0.29575  
       3rd Qu.:0.010   3rd Qu.:0.00325   3rd Qu.:34.00   3rd Qu.: 0.42549  
       Max.   :0.010   Max.   :0.01000   Max.   :45.00   Max.   : 1.07651  
        max_es_prior     mean_es_conf       max_es_conf     mean_es_outcome   
       Min.   :0.5762   Min.   :-0.14245   Min.   :0.4594   Min.   :-0.31513  
       1st Qu.:0.8766   1st Qu.: 0.04001   1st Qu.:0.7327   1st Qu.: 0.07279  
       Median :1.0061   Median : 0.16301   Median :1.0008   Median : 0.29856  
       Mean   :1.2594   Mean   : 0.21951   Mean   :1.0594   Mean   : 0.31220  
       3rd Qu.:1.3609   3rd Qu.: 0.43719   3rd Qu.:1.4116   3rd Qu.: 0.52995  
       Max.   :3.8051   Max.   : 0.84614   Max.   :2.0889   Max.   : 0.97856  
       max_es_outcome   n_unique_enact_years   mu1_prior         mu0_prior     
       Min.   :0.5034   Min.   :11.0         Min.   : 0.3933   Min.   : 1.322  
       1st Qu.:0.8235   1st Qu.:13.0         1st Qu.: 2.1353   1st Qu.: 1.712  
       Median :0.9826   Median :13.0         Median :10.2735   Median : 6.605  
       Mean   :1.1003   Mean   :13.5         Mean   :10.5992   Mean   : 7.187  
       3rd Qu.:1.1509   3rd Qu.:14.0         3rd Qu.:19.0732   3rd Qu.:12.683  
       Max.   :2.8051   Max.   :16.0         Max.   :21.1275   Max.   :14.260  
          sd_prior     mu1_prior_old    mu0_prior_old     sd_prior_old  
       Min.   :3.709   Min.   : 1.596   Min.   : 1.345   Min.   :2.850  
       1st Qu.:5.712   1st Qu.: 2.192   1st Qu.: 1.452   1st Qu.:2.850  
       Median :6.751   Median : 8.226   Median : 5.566   Median :3.948  
       Mean   :6.347   Mean   : 8.519   Mean   : 5.992   Mean   :3.948  
       3rd Qu.:7.386   3rd Qu.:14.786   3rd Qu.:10.548   3rd Qu.:5.045  
       Max.   :8.177   Max.   :15.646   Max.   :11.532   Max.   :5.045  
            mu1             mu0              sd          na.rm              iter  
       Min.   :16.57   Min.   :11.35   Min.   : 7.555   Mode:logical   Min.   :1  
       1st Qu.:19.67   1st Qu.:12.57   1st Qu.: 7.959   TRUE:180       1st Qu.:2  
       Median :20.94   Median :12.95   Median : 8.744                  Median :3  
       Mean   :21.05   Mean   :13.04   Mean   : 8.903                  Mean   :3  
       3rd Qu.:22.37   3rd Qu.:13.74   3rd Qu.: 9.688                  3rd Qu.:4  
       Max.   :25.01   Max.   :14.93   Max.   :10.568                  Max.   :5  
            seed     
       Min.   :9782  
       1st Qu.:9782  
       Median :9782  
       Mean   :9782  
       3rd Qu.:9782  
       Max.   :9782  

