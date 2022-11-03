#' Plot results from non-concurrent & concurrent simulations
#' 
#' @description Takes results from dispatch simulation and provides diagnostic
#' plots comparing results across models and simulations
#'
#' @param sim_results object created from dispatch_simulations
#' @param ui Should the plot include model uncertainty (rather than uncertainty across iterations)?
#' @param draws How many draws of the effect estimate for each iteration?
#' @param ui_alpha What should the uncertainty bounds be within the plots? Takes on a value between 0 & 1, calculating out lower and upper bounds accordingly (for example, 95% UI would correspond to an alpha = 0.05 and plot would display 2.5th percentile and 97.5th percentile)
#' @export


plot_sims <- function(sim_results, ui = T, draws = 100, ui_alpha = 0.05){
  
  # Determine simulation type
    sim_sub <- sim_results %>%
      select(sim_id, mod_id, iter, estimate1, se1, estimate2, se2)
    
    sim_set <- sim_results %>%
      select(model_name, effect_direction, policy_speed, n_implementation_periods,
             rho, years_apart, ordered, effect_magnitude1, effect_magnitude2, sim_id, mod_id) %>%
      unique() 
    
    # Calculate summary statistics across iterations. For a given sim, model, iteration,
    # expand out the estimates into 1000 draws of marginal effect, assuming normal distribution. 
    # Note, this procedure would need to be customized, based on the model formula
    # and transformations. For now, I'm assuming the model formula is including
    # a single beta coefficient for treatment (so no interaction terms or functional
    # forms) and I'm not transforming estimates to the metric space of the 
    # original outcome (in case of glm or other model transforms). 
    
    # I'm basing this on the simpler procedure recommended in King, Tomz & Wittenberg (2000):
    # https://gking.harvard.edu/files/gking/files/making.pdf
    
    # But this is not a straightforward for more complex models. See discussion on 
    # this in the marginal package, here: 
    # https://cran.r-project.org/web/packages/margins/vignettes/TechnicalDetails.pdf)
    # so it would likely be inappropriate to provide a general function for users.
    
    # See also, Gelman and Pardoe 2007, which provides a general procedure for
    # estimating average predictive comparisons from nonlinear models:
    # http://www.stat.columbia.edu/~gelman/research/published/ape17.pdf
    
    # After simulating effect draws from within each iteration, calculate summary 
    # statistics for the mean and uncertainty intervals across iterations.
    
    if (ui){
      
      # Code below is inefficient. Ideally, I would use data.table to 
      # perform below succintly but I'm trying to limit package dependencies
      # since dplyr is already required
      
      df_sim_effects <- expand.grid(sim_id = unique(sim_results$sim_id),
                                    mod_id = unique(sim_results$mod_id),
                                    iter = unique(sim_results$iter),
                                    iter_draw = 1:draws)
      
      df_sim_effects <- merge(df_sim_effects, sim_sub)
      
      df_sim_effects$effect1_draw <- mapply(rnorm, 
                                            mean = df_sim_effects$estimate1,
                                            sd = df_sim_effects$se1,
                                            MoreArgs = list(n = 1))
      
      df_sim_effects$effect2_draw <- mapply(rnorm, 
                                            mean = df_sim_effects$estimate2,
                                            sd = df_sim_effects$se2,
                                            MoreArgs = list(n = 1))
      
      df_sim_effects <- df_sim_effects %>%
                        group_by(sim_id, mod_id) %>%
                        mutate(effect1_mean = median(effect1_draw),
                               effect2_mean = median(effect1_draw),
                               effect1_lower = quantile(effect1_draw, probs = ui_alpha/2),
                               effect2_lower = quantile(effect1_draw, probs = ui_alpha/2),
                               effect1_upper = quantile(effect1_draw, probs = 1 - ui_alpha/2),
                               effect2_upper = quantile(effect1_draw, probs = 1 - ui_alpha/2)) %>%
                        select(sim_id, mod_id, effect1_mean, effect1_lower, 
                               effect1_upper, effect2_mean, effect2_lower, effect2_upper) %>%
                        unique() %>%
                        as.data.frame()
      
    }else{
      
      df_sim_effects <- sim_sub %>%
        group_by(sim_id, mod_id) %>%
        mutate(effect1_mean = median(estimate1),
               effect2_mean = median(estimate2),
               effect1_lower = quantile(estimate1, probs = ui_alpha/2),
               effect2_lower = quantile(estimate2, probs = ui_alpha/2),
               effect1_upper = quantile(estimate1, probs = 1 - ui_alpha/2),
               effect2_upper = quantile(estimate2, probs = 1 - ui_alpha/2)) %>%
        select(sim_id, mod_id, effect1_mean, effect1_lower, 
               effect1_upper, effect2_mean, effect2_lower, effect2_upper) %>%
        unique() %>%
        as.data.frame()
      
    }
    
    # Add on sim settings:
    df_sim_effects <- merge(df_sim_effects, sim_set, all.x = T)
    
  }
  
  # Compare two models for varying policy_speed and rho:
  df_plot <- df_sim_effects %>%
             filter(effect_direction == "neg" & 
                      effect_magnitude1 > 0.4 & 
                      effect_magnitude2 > 0.4)
  
  ggplot(df_plot, aes(x = model_name, y = effect1_mean, 
                      ymin = effect1_lower, ymax = effect2_upper)) +
    geom_pointrange() +
    geom_hline(yintercept = -0.4550925, linetype = 2) +
    facet_grid(policy_speed~rho) +
    coord_flip() +
    theme_bw()
  
}
