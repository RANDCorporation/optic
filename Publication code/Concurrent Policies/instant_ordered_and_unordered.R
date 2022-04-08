# instant_ordered_and_unordered.R -----------------------------------------
# 
# Split the prod-runs-concurrent-weighted-2021-04-19.csv file into
# ordered and unordered runs
#
# Adam Scherling, 2/14/2022


# load libraries ----------------------------------------------------------

library(dplyr)
library(glue)


# load data ---------------------------------------------------------------

inpath <- ''
outpath <- ''

all <- read.csv(glue('{inpath}/prod-runs-concurrent-weighted-2021-04-19.csv'),
                stringsAsFactors = F)

ordered <- all %>% filter(policy_speed=='instant', orderedTx==TRUE)
unordered <- all %>% filter(policy_speed=='instant', orderedTx==FALSE)


# write data --------------------------------------------------------------

write.csv(ordered, glue('{outpath}/instant_ordered.csv'))
write.csv(unordered, glue('{outpath}/instant_unordered.csv'))

