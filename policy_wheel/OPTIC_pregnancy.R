## Project Title: OPTIC State Policy Chart - Substance Use in Pregnancy
## Last Updated by: Seema Choksy (10.1.2019), choksy@healthpolicy.usc.edu

# Load library "circlize"
install.packages("circlize")
library(circlize)

# Define year object
year <- c(2000,2005,2010,2015,2020, 2025)

# Define state_name object
# This is crucial in creating each sector of the circular chart
states <- c("OH", "WI", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "IA", 
            "KS", "MN", "MO", "NE", "ND", "SD", "AL", "KY", "MS", "TN", "AR", "LA", 
            "OK", "TX", "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "CA", "OR", 
            "WA", "AK", "HI", "CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA", 
            "IL", "IN", "MI")

state_levels <- c("OH", "WI", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "IA", 
                  "KS", "MN", "MO", "NE", "ND", "SD", "AL", "KY", "MS", "TN", "AR", "LA", 
                  "OK", "TX", "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "CA", "OR", 
                  "WA", "AK", "HI", "CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA", 
                  "IL", "IN", "MI")

state_name <- factor(states, levels = state_levels)

# Define policy objects, i.e. what states had which policies by year
# 2000
	
	CHILDWELFARE_2000 <- c("FL", "IL", "IN", "IA", "MD", "MN", "OK", "SD", "TX", "VA", "WA", "WI")

	REPORTING_2000 <- c("AZ", "CA", "FL", "IL", "IA", "MI", "MN", "OK", "RI", "UT", "VA", "WI")

	TESTING_2000 <-c("IA", "KY", "MN", "RI")
	
	TARGETED_2000 <-c("AR", "CA", "CO", "CT", "FL", "IL", "KY", "LA", "MD", "NE", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2000 <-c("AZ", "AR", "DE", "DC", "FL", "GA", "IL", "KS", "LA", "MD", "MO", "OH", "OK", 
				"TX", "WI")

	PROTECTED_2000 <-c("FL", "IL", "IA", "KS", "LA", "MO", "OH", "OK")

# 2005
   
 	CHILDWELFARE_2005 <- c("AR", "CO", "DC", "FL", "IL", "IN", "IA", "LA", "MD", "MN", "NV", "ND", 
				     "OK", "SD", "TX", "VA", "WA", "WI")

	REPORTING_2005 <- c("AZ", "CA", "DC", "FL", "HI", "IL", "IA", "ME", "MI", "MN", "NV", "ND", "OK", 
				  "RI", "UT", "VA", "WI")

	TESTING_2005 <-c("IA", "KY", "MN", "ND", "RI")
	
	TARGETED_2005 <-c("AR", "CA", "CO", "CT", "FL", "IL", "KY", "LA", "MD", "NE", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2005 <-c("AZ", "AR", "DE", "DC", "FL", "GA", "IL", "IA", "KS", "LA", "ME", "MD", "MO", 
				"OH", "OK", "TX", "WI")

	PROTECTED_2005 <-c("FL", "IL", "IA", "KS", "LA", "MO", "OH", "OK")


# 2010
   
 	CHILDWELFARE_2010 <- c("AL", "AZ", "AR", "CO", "DC", "FL", "IL", "IN", "IA", "LA", "MD", "MN", "NV", "ND", 
				     "OK", "SC", "SD", "TX", "VA", "WA", "WI")

	REPORTING_2010 <- c("AK", "AZ", "CA", "DC", "FL", "HI", "IL", "IA", "LA", "ME", "MA", "MI", "MN", 
				  "MT", "NV", "ND", "OK", "PA", "RI", "UT", "VA", "WI")

	TESTING_2010 <-c("IA", "KY", "LA", "MN", "ND", "RI")
	
	TARGETED_2010 <-c("AR", "CA", "CO", "CT", "FL", "IL", "KY", "LA", "MD", "MN", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2010 <-c("AZ", "AR", "DE", "DC", "FL", "GA", "IL", "IA", "KS", "LA", "ME", "MD", "MO", 
				"OH", "OK", "TX", "UT", "WI")

	PROTECTED_2010 <-c("FL", "IL", "IA", "KS", "LA", "MO", "OH", "OK")

# 2015
   
 	CHILDWELFARE_2015 <- c("AL", "AZ", "AR", "CO", "DC", "FL", "IL", "IN", "IA", "LA", "MD", "MN", "MO", 
				     "NV", "ND", "OK", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "WA", "WI")

	REPORTING_2015 <- c("AK", "AZ", "CA", "DC", "FL", "IL", "IA", "KY", "LA", "ME", "MD", "MA", "MI", 
				  "MN", "MT", "NV", "ND", "OK", "PA", "RI", "UT", "VA", "WI")

	TESTING_2015 <-c("IN", "IA", "KY", "LA", "MN", "ND", "RI")
	
	TARGETED_2015 <-c("AR", "CA", "CO", "CT", "FL", "IL", "IN", "KY", "LA", "MD", "MN", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2015 <-c("AL", "AZ", "AR", "DE", "DC", "FL", "GA", "IL", "IA", "KS", "KY", "LA", "ME", "MD", 
				"MO", "OH", "OK", "TN", "TX", "UT", "WI") 

	PROTECTED_2015 <-c("AL", "FL", "IL", "IA", "KS", "KY", "LA", "MO", "OH", "OK", "TN")

# 2020: placeholder
   
 	CHILDWELFARE_2020 <- c("AL", "AZ", "AR", "CO", "DC", "FL", "IL", "IN", "IA", "LA", "MD", "MN", "MO", 
				     "NV", "ND", "OK", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "WA", "WI")

	REPORTING_2020 <- c("AK", "AZ", "CA", "DC", "FL", "IL", "IA", "KY", "LA", "ME", "MD", "MA", "MI", 
				  "MN", "MT", "NV", "ND", "OK", "PA", "RI", "UT", "VA", "WI")

	TESTING_2020 <-c("IN", "IA", "KY", "LA", "MN", "ND", "RI")
	
	TARGETED_2020 <-c("AR", "CA", "CO", "CT", "FL", "IL", "IN", "KY", "LA", "MD", "MN", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2020 <-c("AL", "AZ", "AR", "DE", "DC", "FL", "GA", "IL", "IA", "KS", "KY", "LA", "ME", "MD", 
				"MO", "OH", "OK", "TN", "TX", "UT", "WI") 

	PROTECTED_2020 <-c("AL", "FL", "IL", "IA", "KS", "KY", "LA", "MO", "OH", "OK", "TN")

# 2025: placeholder
   
 	CHILDWELFARE_2025 <- c("AL", "AZ", "AR", "CO", "DC", "FL", "IL", "IN", "IA", "LA", "MD", "MN", "MO", 
				     "NV", "ND", "OK", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "WA", "WI")

	REPORTING_2025 <- c("AK", "AZ", "CA", "DC", "FL", "IL", "IA", "KY", "LA", "ME", "MD", "MA", "MI", 
				  "MN", "MT", "NV", "ND", "OK", "PA", "RI", "UT", "VA", "WI")

	TESTING_2025 <-c("IN", "IA", "KY", "LA", "MN", "ND", "RI")
	
	TARGETED_2025  <-c("AR", "CA", "CO", "CT", "FL", "IL", "IN", "KY", "LA", "MD", "MN", "NY", "NC", "OH", 
				"OR", "PA", "SC", "TN", "VA", "WA", "WI")

	PRIORITY_2025 <-c("AL", "AZ", "AR", "DE", "DC", "FL", "GA", "IL", "IA", "KS", "KY", "LA", "ME", "MD", 
				"MO", "OH", "OK", "TN", "TX", "UT", "WI") 

	PROTECTED_2025 <-c("AL", "FL", "IL", "IA", "KS", "KY", "LA", "MO", "OH", "OK", "TN")


# Specify details of the png file the chart will be exporting as
png(filename = "C:/Users/Seema/Documents/R/optic/img/OPTIC_pregnancy.png", 
    width = 500, height = 400, units='mm', res = 300)


# Create layout onto which the chart's title, legend, and policy wheels will be pasted onto
layout.mat <- matrix(c(1,4,2,5,3,6), ncol=3)               # plot matrix
layout.mat <- rbind(matrix(7, nrow=1, ncol=3), layout.mat) # space for the title
layout.mat <- rbind(layout.mat, matrix(8, nrow=1, ncol=3)) # space for the legend

layout(layout.mat, respect=TRUE, heights = c(0.35,3,3,0.35), widths = c(3,3,3))
#layout.show(8)

# Create & plot the policy wheels
for(i in unique(year)) {
  # Establish margins & cell padding
  par(mai=c(0.35,0.35,0.35,0.35), xpd=TRUE)
  circos.par(cell.padding = c(0, 0, 0, 0))
  
  # Add sectors (i.e. each state + DC)
  circos.initialize(factors = state_name, xlim = c(0, 1))
  
  # Add tracks (i.e. each policy)
  for(j in 1:6) {
    circos.track(ylim = c(0,1), track.height = 0.09)
  }
  
  # Add state labels
  for(j in unique(states)) {
    highlight.sector(sector.index = j, track.index = 1, 
                     text = j, padding = c(-.5,1,1.5,1), cex = 1.3, text.vjust = .5, col = NA, facing = "downward")
  }
  
  # Label each graph with the correct year (in the middle of the circles)
  text(0,0, labels = print(i), cex = 2.8)
  
  # Add line segments to distinguish between regions
  for(j in c("CT", "NY", "IL", "DE", "IA", "AL", "AR", "AZ", "CA")) {
    circos.segments(x0 = -0.1, y0 = 0, x1 = -0.07, y1 = 5.8, lwd = 3.5, sector.index = j)
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
  
  # Highlight states that had effective legislation by Jan 1 of that year
  if(i==2000) {
    for (j in CHILDWELFARE_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 1),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1),
                  col = "#b20000")
    }
    
    for (j in REPORTING_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 2),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 2),
                  col = "#ff0000")
    }
    
    for (j in TESTING_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 3),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 3),
                  col = "#ff6666")
    }
        
      for (j in TARGETED_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 4),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 4),
                  col = "#004c00")
    }

  	for (j in PRIORITY_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 5),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 5),
                  col = "#008000")
    }
	for (j in PROTECTED_2000) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 6),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 6),
                  col = "#66b266")
    }
  }

  
  if(i==2005) {
    for (j in CHILDWELFARE_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 1),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1),
                  col = "#b20000")
    }
    
    for (j in REPORTING_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 2),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 2),
                  col = "#ff0000")
    }
    
    for (j in TESTING_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 3),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 3),
                  col = "#ff6666")
    }
        
      for (j in TARGETED_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 4),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 4),
                  col = "#004c00")
    }

  	for (j in PRIORITY_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 5),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 5),
                  col = "#008000")
    }
	for (j in PROTECTED_2005) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 6),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 6),
                  col = "#66b266")
    }
  }

    
   if(i==2010) {
    for (j in CHILDWELFARE_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 1),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1),
                  col = "#b20000")
    }
    
    for (j in REPORTING_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 2),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 2),
                  col = "#ff0000")
    }
    
    for (j in TESTING_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 3),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 3),
                  col = "#ff6666")
    }
        
      for (j in TARGETED_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 4),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 4),
                  col = "#004c00")
    }

  	for (j in PRIORITY_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 5),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 5),
                  col = "#008000")
    }
	for (j in PROTECTED_2010) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 6),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 6),
                  col = "#66b266")
    }
  }

  if(i==2015) {
    for (j in CHILDWELFARE_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 1),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1),
                  col = "#b20000")
    }
    
    for (j in REPORTING_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 2),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 2),
                  col = "#ff0000")
    }
    
    for (j in TESTING_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 3),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 3),
                  col = "#ff6666")
    }
        
      for (j in TARGETED_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 4),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 4),
                  col = "#004c00")
    }

  	for (j in PRIORITY_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 5),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 5),
                  col = "#008000")
    }
	for (j in PROTECTED_2015) {
      draw.sector(get.cell.meta.data("cell.start.degree", sector.index = j),
                  get.cell.meta.data("cell.end.degree", sector.index = j),
                  rou1 = get.cell.meta.data("cell.top.radius", track.index = 6),
                  rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 6),
                  col = "#66b266")
    }
  }
	circos.clear()
}

# Add title
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
ll <- par("usr")
#rect(ll[1], ll[3], ll[2], ll[4])
text(0.5, 0.5, "State Substance Use in Pregnancy Policies, 2000, 2005, 2010, 2015", cex=2)

# Add legend
#par(xpd=TRUE) 
#plot.new()
#plot.window(xlim=c(0,1), ylim=c(0,1))
#ll <- par("usr")
#rect(ll[1], ll[3], ll[2], ll[4])
#legend("center", 
#       pch = c(15,15,15,15,15,8), 
#       legend = c("Child welfare", 
#                  "Reporting", 
#                  "Testing",
#			"Targeted program",
#			"Priority access"), 
#       col = c("#b20000", "#ff0000", "#ff6666", "#004c00", "#008000"),
#       horiz = TRUE, xjust = 0.5, x.intersp = 1, cex = 1.8, pt.cex = 3.8,
#       text.width=c(0.16, 0.146, 0.142, 0.142, 0.135, 0.085), bty = "n")

dev.off()

