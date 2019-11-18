library(future)

workers <- rep(c("poppy.rand.org"), each=16)

cl <- makeClusterPSOCK(
  workers=workers,
  user="ggrimm",
  rscript="/usr/bin/Rscript"
)

plan(cluster, workers=cl)

r <- list()
for(i in 1:16) {
  r[[paste0("iter", i)]] <- future({
    for(j in 1:1000) {
      zz <- lm(ALLDEATHS ~ STATE + factor(YEAR) + CIVLABORFORCEASPERCENTTOTALPOPUL, data=x)
    }
    return(paste0(Sys.info()[["nodename"]], ": ", Sys.getpid()))
  })
}

for (i in 1:length(r)){
  print(value(r[[i]]))
}
