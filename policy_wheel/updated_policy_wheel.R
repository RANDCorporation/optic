# Updated policy wheel for OPTIC project 2022
# Max Griswold
# 12/5/22

library(circlize)
library(data.table)

setwd("C:/users/griswold/Documents/GitHub/optic/policy_wheel/")

# Load data and reshape long. Recode policies as absorbing states at 5-year 
# intervals

df <- fread('bup_policy_state_year.csv')
df <- melt(df, id.vars = "state", variable.name = "policy", value.name = "year")

# Set up plot options and hardcode plot ordering
plot_colors <- c("#5e3c99", "#b2abd2", 
                 "#a6611a", "#dfc27d", 
                 "#0571b0", "#80cdc1")

policy_intervals <- c(2010, 2015, 2018)

# Order states so that region-names make sense when applied to areas of the policy circle:
states <- c("OH", "WI", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "IA", 
            "KS", "MN", "MO", "NE", "ND", "SD", "AL", "KY", "MS", "TN", "AR", "LA", 
            "OK", "TX", "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "CA", "OR", 
            "WA", "AK", "HI", "CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA", 
            "IL", "IN", "MI")

# Ordering policies to match PI requests:
policies <- c("CME", "Additional training", "ACA", 
              "Medicaid BUP coverage", "Mandatory PDMP", "Pain clinic law")


# Set up square dataset, then subset policy database to rows with observations.
# Merge observations onto the square dataset.

df_square <- expand.grid("state" = states,
                         "policy" = policies,
                         "year" = 2004:2020)

df <- df[!is.na(year),]
df[, implemented := 1]

df <- setDT(merge(df_square, df, by = c("state", "policy", "year"), all.x = T))

# Set implemented == T, for all years after the implementation year:
setorder(df, state, policy, year)
df[, implemented := nafill(.SD$implemented, "locf"), by = c("state", "policy")]

#Restrict to relevant policy intervals, for locations that implemented the policy
df <- df[(year %in% policy_intervals) & (implemented == 1),]

# Set up dictionary for policy wheel options:
wheel_opts <- data.table("policy" = policies,
                         "i" = 1:6,
                         "col" = plot_colors)


# Below fills in policy wheel cells by year and policy
fill_in_cells <- function(p, y){
  
  state_implementors <- unique(df[year == y & policy == p]$state)
  
  track_index <- wheel_opts[policy == p]$i
  track_color <- wheel_opts[policy == p,]$col
  
  for (s in state_implementors){
    draw.sector(get.cell.meta.data("cell.start.degree", sector.index = s),
                get.cell.meta.data("cell.end.degree", sector.index = s),
                rou1 = get.cell.meta.data("cell.top.radius", track.index = track_index),
                rou2 = get.cell.meta.data("cell.bottom.radius", track.index = track_index),
                col = track_color)
  }
}

plot_policy_wheel <- function(y){
  
  # Establish margins & cell padding
  par(mai=c(0.35, 0.35, 0.35, 0.35), xpd = TRUE)
  
  circos.par(cell.padding = c(0, 0, 0, 0))
  
  # Add fifty-one sectors (i.e. one for each state + DC)
  circos.initialize(factors = states, xlim = c(0, 1))
  
  # Add six tracks (i.e. one for each policy)
  replicate(6, circos.track(ylim = c(0,1), track.height = 0.09), simplify = F)
  
  # Add state labels
  for(s in unique(states)) {
    highlight.sector(sector.index = s, track.index = 1,
                     text = s, padding = c(-.5,1,1.5,1), cex = 1.3, text.vjust = .5, col = NA, facing = "downward")
  }
  
  # Label each graph with the correct year (in the middle of the circles)
  text(0,0, labels = print(y), cex = 2.8)
  
  # Add line segments to distinguish between regions
  for(s in c("CT", "NY", "IL", "DE", "IA", "AL", "AR", "AZ", "CA")) {
    circos.segments(x0 = -0.1, y0 = 0, x1 = -0.07, y1 = 5.8, lwd = 3.5, sector.index = s)
  }
  
  # Label the regions
  # New England
  highlight.sector(sector.index = c("CT", "ME", "MA", "NH", "RI", "VT"), track.index = 1,
                   text = "New England", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.inside")
  
  # Mid-Atlantic
  highlight.sector(sector.index = c("NY", "NJ", "PA"), track.index = 1,
                   text = "Mid-Atlantic", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.inside")
  
  # East North Central
  highlight.sector(sector.index = c("IL", "IN", "MI", "OH", "WI"), track.index = 1,
                   text = "East North Central", padding = c(1,0,5,0), cex = 1.4,
                   font = 2, border = NA, col = NA, facing = "bending.inside")
  
  # South Atlantic
  highlight.sector(sector.index = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV"),
                   track.index = 1, text = "South Atlantic", padding = c(1,0,5,0),
                   cex = 1.4, font = 2, border = NA, col = NA, facing = "bending.outside")
  
  # West North Central
  highlight.sector(sector.index = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
                   track.index = 1, text = "West North Central", padding = c(1,0,5,0),
                   cex = 1.4, font = 2, border = NA, col = NA, facing = "bending.outside")
  
  # East South Central
  highlight.sector(sector.index = c("AL", "KY", "MS", "TN"), track.index = 1,
                   text = "East South Central", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.outside")
  
  # West South Central
  highlight.sector(sector.index = c("AR", "LA", "OK", "TX"), track.index = 1,
                   text = "West South Central", padding = c(1,0,5,0), cex = 1.4, font = 2,
                   border = NA, col = NA, facing = "bending.outside")
  
  # Mountain
  highlight.sector(sector.index = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
                   track.index = 1, text = "Mountain", padding = c(1,0,5,0), cex = 1.4,
                   font = 2, border = NA, col = NA, facing = "bending.inside")
  
  # Pacific
  highlight.sector(sector.index = c("CA", "OR", "WA", "AK", "HI"), track.index = 1,
                   text = "Pacific", padding = c(1,0,5,0), cex = 1.4,
                   border = NA, col = NA, font = 2, facing = "bending.inside")
  
  # Highlight states that implemented legislation by Jan 1 of that year
  lapply(policies, fill_in_cells, y = y)

  circos.clear()
  
}

svg(filename = "policy_wheel_new_2018.svg",
    width = 10, height = 10)

# Create layout onto which the chart's title, legend, and policy wheels will be pasted onto
#layout.mat <- matrix(c(1,3,2,3), ncol=2)               # plot matrix
#layout.mat <- rbind(matrix(4, nrow=1, ncol=2), layout.mat) # space for the title
#layout.mat <- rbind(layout.mat, matrix(5, nrow=1, ncol=2)) # space for the legend

#layout(layout.mat, respect = TRUE, heights = c(0.35, 3, 3, 0.35), widths = c(3, 3))

lapply(policy_intervals[[3]], plot_policy_wheel)

dev.off()

# Add title
# plot.new()
# plot.window(xlim=c(0,1), ylim=c(0,1))
# ll <- par("usr")

#text(0.5, 0.5, "Not sure what title should be", cex=2)

svg(filename = "policy_wheel_legend.svg",
    width = 10, height = 2)

# Add legend in correct order of colors:
col_order <- matrix((3*2):1, nrow = 3, ncol = 2, byrow = T)

par(xpd=TRUE)
plot.new()
plot.window(xlim = c(0,1.5), ylim = c(0,2.5))
ll <- par("usr")
legend("center",
      pch = c(15, 15, 15, 15, 15, 15),
      legend = wheel_opts$policy[col_order],
      col = wheel_opts$col[col_order],
      xjust = 0.5, y.intersp = 1.3, 
      x.intersp = 1.3, cex = 1.8, 
      pt.cex = 2.7, bty = "n", ncol = 2)

