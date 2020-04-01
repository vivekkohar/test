# 
# library(sRACIPE)
# library(data.table)
# data("demoCircuit")
# 
# racipe <- sracipeSimulate(demoCircuit, nIC = 20, numModels = 100,plots = FALSE)
# assay <- assay(racipe)
# 
# tmp <- t(assay)
# tmp <- as.matrix(dist(tmp))
# nGenes <- 2
# tolerance <- 0.001*sqrt(nGenes)
# getState(tmp,tolerance)
# 
# simData <- data.table(t(assay(racipe)))
# config <- sracipeConfig(racipe)
# simData$IC <- rep(seq(1,config$simParams[["nIC"]]),config$simParams[["numModels"]]/config$simParams[["nIC"]])
# simData$Model <- rep(seq(1,config$simParams[["numModels"]]/config$simParams[["nIC"]]),each = config$simParams[["nIC"]])
# DT <- simData
# tmp <- DT[, as.matrix(dist(DT[,1:2])), by = Model] 
# 
# tmp2 <- DT[, table(B), by = Model] 
