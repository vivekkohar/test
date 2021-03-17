library("doFuture")
registerDoFuture()
plan(multiprocess)
## Not supported on Rstudio
mu <- 1.0
sigma <- 2.0
x <- foreach(i = 1:3,mu=c(1,100,1000),sigma = c(1,1,1), .export = c("mu", "sigma")) %dopar% {
  rnorm(i, mean = mu, sd = sigma)
}
tmp3 <- list(10,200,30,40)
tmp2 <- foreach(i=1:4, j=tmp3,k=list(100,200,300,300,500)) %dopar% (i+j+k)


data("demoCircuit")
t <- Sys.time()
tmp <- sracipeSimulate(demoCircuit, numModels = 10000)
Sys.time() -t

t <- Sys.time()
tmp2 <- sracipeSimulate(demoCircuit, numModels = 10000, nCores = 4)
Sys.time() -t

data("EMT2")
t <- Sys.time()
tmp2 <- sracipeSimulate(EMT2, numModels = 1000, nCores = 8)
Sys.time() -t

t <- Sys.time()
tmp2 <- sracipeSimulate(EMT2, numModels = 1000, nCores = 1)
Sys.time() -t

