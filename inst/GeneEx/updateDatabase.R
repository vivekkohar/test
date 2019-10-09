updateDatabase <- function(){
  tpoFiles <- list.files(path = "inputs/", pattern = ".tpo", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  tpoFilesNames <- tools::file_path_sans_ext(tpoFiles)
  tpoFilesDatabase <- data.frame(Circuit = tpoFilesNames)
  for(i in 1:length(tpoFilesNames)){
    #print(i)
    rs <- read.table(paste0("inputs/",tpoFiles[i]))
    geneNames[[i]] <- (varMetadata(rs@featureData)$labelDescription)
  }
  database$Genes <- geneNames
  saveRDS(database, "inst/shiny-examples/GeneVyuha/data/database.RDS")
}

storedNetworks <- function(){
  databaseFiles <- list.files(path = "inst/shiny-examples/GeneVyuha/inputs/", pattern = ".tpo", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  databaseFileNames <- tools::file_path_sans_ext(databaseFiles)
  databaseFileNames <- append("None", databaseFileNames)
  database <- data.frame(Network = databaseFileNames)

  saveRDS(database, "inst/shiny-examples/GeneVyuha/data/storedNetworks.RDS")
}

