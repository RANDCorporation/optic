# Calculate summary statistics for OPTIC winter workshop

library(data.table)  

setwd("C:/users/griswold/documents/Github/Optic/class_materials/")

df <- fread("winter_workshop_sim_results.csv")
df_add <- fread("optic_winter_webinar_ar_only.csv")

df <- rbind(df, df_add)

# Beth Ann only wanted results for the following model:
# TE = 5%; N = 30; Speed = Instant

df <- df[policy_speed == "instant" & n_units == 30 & effect_magnitude <= 1,]

# Calculate the following statistics:

# Mean of estimate
# Mean of estimate se
# Coverage of true effect
# Mean bias
# Variance of estimate
# MSE across iterations

df[, mean_model_estimate := mean(.SD$estimate), by = "model_name"]
df[, mean_model_se := mean(.SD$se), by = "model_name"]

df[, mean_model_bias := mean(.SD$effect_magnitude - .SD$estimate), by = "model_name"]

df[, coverage := ifelse(estimate + 1.96*se >= effect_magnitude & estimate - 1.96*se <= effect_magnitude, 1, 0)]
df[, mcmc_coverage := sum(.SD$coverage)/.N, by = "model_name"]

df[, mcmc_var := var(.SD$estimate), by = "model_name"]

df[, mcmc_mse := mean((.SD$effect_magnitude - .SD$estimate)^2), by = "model_name"]

df_summary <- unique(df[, .(model_name, mean_model_estimate, mean_model_se, mean_model_bias, mcmc_coverage, mcmc_var, mcmc_mse)])

write.csv(df_summary, "winter_workshop_summary_stats.csv", row.names = F)

# Some of the results above are unexpected to me; look at some plots to interpret these results further:

# Why is coverage higher for ASCM despite a much smaller MCMC variance in estimated effects?

# First: Does this variance seem reasonable?
plot(df[model_name == "did"]$se, df[model_name == "did"]$estimate, col = 'blue', ylab = "Estimate", xlab = "se")
points(df[model_name == "ASCM"]$se, df[model_name == "did"]$estimate, col = 'red')
abline(b = 0, a = unique(df$effect_magnitude), lty = 'dotted')
legend("topright", legend = c("DID", "ASCM"), col = c("blue", "red"), pch = c(1, 1))

# Density is tighter and bias does seem higher for ASCM. How much smaller is the se
# within ASCM models compared to DID?

plot(density(df[model_name == "did"]$estimate), col = "blue", main = "Density of estimated effects", ylim = c(0, 1))
lines(density(df[model_name == "ASCM"]$estimate), col = "red")
abline(v = unique(df$effect_magnitude), lty = 'dotted')
legend("topright", legend = c("DID", "ASCM"), col = c("blue", "red"), pch = c(1, 1))

plot(density(df[model_name == "did"]$se), col = "blue", main = "Density of effect se", ylim = c(0, 5), xlim = c(0, 3))
lines(density(df[model_name == "ASCM"]$se), col = "red")
legend("topright", legend = c("DID", "ASCM"), col = c("blue", "red"), pch = c(1, 1))

# Given the smaller SE, and more biased effects, how does this translate into coverage for ASCM
# compared to DID?

par(mfrow = c(1, 2), xpd = T)
for (mod in c("did", "ASCM")){
  color <- ifelse(mod == "did", "blue", "red")
  title <- ifelse(mod == "did", "Coverage", "")
  plot(range(-8, 8, 2),
       range(0, 100), type = 'n', main = title, xlab = "CI", ylab = "Iter")
  segments(df[model_name == mod]$estimate - 1.96*df[model_name == mod]$se,
           df[model_name == mod]$iter,  
           df[model_name == mod]$estimate + 1.96*df[model_name == mod]$se,
           df[model_name == mod]$iter, lty = 'dotted', col = color)
  segments(unique(df$effect_magnitude), 0, unique(df$effect_magnitude), 100)
  if (mod == "ASCM"){
    legend("topright", legend = c("DID", "ASCM"), col = c("blue", "red"), pch = c(1, 1), inset = c(0, -0.2))
  }
}

# Based on above plot, while the average DID model is less biased, 
# the iterations with large bias are way off and do not have proportionally larger variance. 

