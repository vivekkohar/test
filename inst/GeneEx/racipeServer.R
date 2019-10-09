
###########################################
# RACIPE
###########################################

# Use a flag 
annealFlag <- FALSE

rsSRsetAnneal <- RacipeSE()
RacipeSet <- RacipeSE()
sRacipeSet <- RacipeSE()


reactVariables$shinyRacipeNetwork <- reactive({
    if(input$importCircuit ==0) { return ()}
})

  observeEvent(input$importCircuit,
               {
                   if(is.null(reactVariables$explorerNetwork())) {output$circuitMsg <- renderUI({HTML(
                     "Please set the network in the GeneVyuha panel first.")})
                   return()
                   } else
                   {
                   show("racipeNetwork")
                   show("parameterRange")
                   show("simTimeRacipe")
                   show("stepSizeRacipe")
                   show("numModels")
                   show("simulateRacipe")
                   show("stochasticRacipe")
                   annealFlag <- FALSE
                   reactVariables$shinyRacipeNetwork <- reactive(reactVariables$explorerNetwork())

                   }
               })

output$racipeNetwork <- renderVisNetwork({
  if(is.null(reactVariables$shinyRacipeNetwork()) ){ return ()}
  rsRacipe <- RacipeSE()
#  annotation(rsRacipe) <- paste0(Sys.Date(),"_",input$filenameTopo)
  sracipeCircuit(rsRacipe) <- reactVariables$shinyRacipeNetwork()

  sracipePlotCircuit(rsRacipe, plotToFile = FALSE)
#  RacipeSet <<- rsRacipe
})



observeEvent(input$simulateRacipe, {
  show("racipeDeterministicText")
  show("racipeHeatmap")
  show("racipePca")
  show("parametricAnalysisRacipe")
  show("h5")
  show("filteredOutputRacipe")
  show("filterSliderRacipe")
  show("filteredOutputRacipe2")
  show("filterSliderRacipe2")
  show("filteredOutputRacipe3")
  show("filterSliderRacipe3")
  show("downloadDataRacipe")
  show("downloadDataRacipeType")
  show("saveDataRacipe")
  show("racipeHeatmapFiltered")
  show("racipePcaFiltered")

  rsRacipe <- RacipeSE()
  annotation(rsRacipe) <- paste0(Sys.Date(),"_",input$filenameTopo)
  sracipeCircuit(rsRacipe) <- isolate(reactVariables$shinyRacipeNetwork())
  sracipeConfig(rsRacipe)$simParams["integrateStepSize"] <- input$stepSizeRacipe
  sracipeConfig(rsRacipe)$simParams["simulationTime"] <-   input$simTimeRacipe
  sracipeConfig(rsRacipe)$stochParams["numModels"] <- input$numModels
  sracipeConfig(rsRacipe)$stochParams["paramRange"] <-   input$parameterRange
 # anneal <- switch(input$sRacipeOption,
#                   constantNoise = FALSE,
#                   annealing = TRUE,
#                   FALSE)

  #      rs <- shinyModelExplorer()
  withProgress(message = 'Simulating', value = 0.25, {

  rsRacipe <- sracipeSimulate(rsRacipe)
  rsRacipe <- sracipeNormalize(rsRacipe)
  RacipeSet <<- rsRacipe
  })
  output$racipeDeterministicText <- renderUI({HTML(
    "Hierarchical clustering and principal component analysis of deterministic simulations ")})


  output$racipePca <- renderPlot({
    if(input$simulateRacipe == 0) return()
    withProgress(message = 'Plotting pca', value = 0.25, {
      plotData <- assay(rsRacipe)
        .sracipePlotPca(plotData)
    })
  })
  output$racipeHeatmap <- renderPlot({
    if(input$simulateRacipe == 0) return()
    withProgress(message = 'Hierarchical clustering', value = 0.25, {
      plotData <- assay(rsRacipe)
      .sracipePlotHeatmap(plotData)
    })
  })
  ###########################################
  # Parametric Analysis
  ###########################################
  observeEvent(input$parametricAnalysisRacipe, {


  if(!is.null(rsRacipe)){
    parameterNames <- sracipeGenParamNames(rsRacipe)
    parameters <- sracipeParams(rsRacipe)
  }
  output$filteredOutputRacipe <- renderUI({
    if(is.null(parameters)) return(NULL)

    selectInput("selectedParameter", "Parameter",
                parameterNames,
                selected = NULL)
  })

  output$filteredOutputRacipe2 <- renderUI({
    if(is.null(parameters)) return(NULL)

    selectInput("selectedParameter2", "Parameter",
                parameterNames,
                selected = NULL)
  })

  output$filteredOutputRacipe3 <- renderUI({
    if(is.null(parameters)) return(NULL)

    selectInput("selectedParameter3", "Parameter",
                parameterNames,
                selected = NULL)
  })

  dataSimulation <- assay(rsRacipe)[[1]] #sracipeNormalize(rsRacipe)
  pca = prcomp(dataSimulation, scale. = F, center = F)


  filtered <- reactive({
    if (is.null(dataSimulation)) {
      return(NULL)
    }
    dataSimulation[,((parameters[,input$selectedParameter] >= 0.01*(input$parameterInput[1]*max(parameters[,input$selectedParameter]))) & (parameters[,input$selectedParameter] < 0.01*(input$parameterInput[2]*max(parameters[,input$selectedParameter]))) &
                    (parameters[,input$selectedParameter2] >= 0.01*(input$parameterInput2[1]*max(parameters[,input$selectedParameter2]))) & (parameters[,input$selectedParameter2] < 0.01*(input$parameterInput2[2]*max(parameters[,input$selectedParameter2]))) &
                    (parameters[,input$selectedParameter3] >= 0.01*(input$parameterInput3[1]*max(parameters[,input$selectedParameter3]))) & (parameters[,input$selectedParameter3] < 0.01*(input$parameterInput3[2]*max(parameters[,input$selectedParameter3])))
                    )]

  })


  output$filterSliderRacipe <- renderUI({
    if(is.null(parameters)) return(NULL)
    sliderInput("parameterInput", "Parameter Range", min = 0,
                max = 100, value = c(0,100))
  })

  output$filterSliderRacipe2 <- renderUI({
    if(is.null(parameters)) return(NULL)
    sliderInput("parameterInput2", "Parameter Range", min = 0,
                max = 100, value = c(0,100))
  })


  output$filterSliderRacipe3 <- renderUI({
    if(is.null(parameters)) return(NULL)
    sliderInput("parameterInput3", "Parameter Range", min = 0,
                max = 100, value = c(0,100))
  })


  output$racipePcaFiltered <- renderPlot({
    if(is.null(filtered())) return(NULL)
    pcaData <- scale(filtered(), pca$center, pca$scale) %*% pca$rotation
    pcaData <- data.frame(PC1=pcaData[,1],PC2=pcaData[,2])
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    plotColor <- rf(32)
    binCount <- 40
    .sracipePlotDensity(pcaData,binCount,plotColor)
  })

  output$racipeHeatmapFiltered <- renderPlot({
    if(is.null(filtered())) return(NULL)
    withProgress(message = 'Hierarchichal clustering', value = 0.25, {
      .sracipePlotHeatmap(filtered())
  })
  })

})

  ########################### Download and Upload ########################
  output$downloadDataRacipe <- downloadHandler(
    filename = function(){
      if(input$downloadDataRacipeType == "txt"){
        return(paste0(annotation(RacipeSet),".zip"))}
      else return(paste0(annotation(RacipeSet),".RDS"))
    },

    content = function(file){
      if(input$downloadDataRacipeType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        fileName <- paste(annotation(RacipeSet),"_GE",".txt",sep = "")
        write.table(assay(RacipeSet),fileName,sep = ' ', row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        # fileName <- paste(annotation(RacipeSet),"_ic",".csv",sep = "")
        # write.table(ic(RacipeSet),fileName,sep = ',', row.names = F, col.names = T)
        # files <- c(fileName,files)

        fileName <- paste(annotation(RacipeSet),"_network",".txt",sep = "")
        write.table(sracipeCircuit(RacipeSet),fileName,sep = ' ', 
                    row.names = FALSE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(annotation(RacipeSet),"_params",".txt",sep = "")
        write.table(sracipeParams(RacipeSet),fileName,sep = ' ', 
                    row.names = TRUE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)
      }
      else{

        saveRDS(RacipeSet, file)

      }


    })

  observeEvent(input$saveDataRacipe,{
    show("uploadDataRacipe")
    show("uploadToDatabaseUI_name_Racipe")
    show("uploadToDatabaseUI_lab_Racipe")
    show("uploadToDatabaseUI_contact_Racipe")
    show("uploadToDatabaseUI_title_Racipe")
    show("uploadToDatabaseUI_abstract_Racipe")
    show("uploadToDatabaseUI_url_Racipe")
    show("uploadToDatabaseUI_pubMedIds_Racipe")

  })

  observeEvent(input$uploadDataRacipe,{
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_Racipe
    tmp@lab <- input$uploadToDatabaseUI_lab_Racipe
    tmp@contact <- input$uploadToDatabaseUI_contact_Racipe
    tmp@title <- input$uploadToDatabaseUI_title_Racipe
    tmp@abstract <- input$uploadToDatabaseUI_abstract_Racipe
    tmp@url <- input$uploadToDatabaseUI_url_Racipe
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_Racipe

    experimentData(RacipeSet) <- tmp
    saveRDS(RacipeSet, file = paste0("usrDatabase/",annotation(rsRacipe),"_RACIPE.RDS"))
    output$fileDataRacipe <- renderText(HTML("File uploaded to Database"))
    show("fileDataRacipe")
  })
})
###########################################
# sRACIPE
###########################################
observeEvent(input$stochasticRacipe, {
  show("sRacipeOption")
  show("sRacipeNoise")
  show("simulateSRacipe")
  show("sRacipeHeatmap")
  show("sRacipePca")
  show("saveDataSRacipe")
  show("downloadDataSRacipe")
  show("downloadDataSRacipeType")

  observeEvent(input$simulateSRacipe, {
#  show("sRacipeHeatmap")
#  show("sRacipePca")
if(!annealFlag){
  rsSRacipe <- RacipeSE()
  annotation(rsSRacipe) <- paste0(Sys.Date(),"_",input$filenameTopo)
  sracipeCircuit(rsSRacipe) <- reactVariables$shinyRacipeNetwork()


  sracipeConfig(rsSRacipe)$simParams["integrateStepSize"] <- input$stepSizeRacipe
  sracipeConfig(rsSRacipe)$simParams["simulationTime"] <-   input$simTimeRacipe
  sracipeConfig(rsSRacipe)$stochParams["numModels"] <- input$numModels
  sracipeConfig(rsSRacipe)$stochParams["paramRange"] <-   input$parameterRange

  sracipeConfig(rsSRacipe)$stochParams["nNoise"] <- 20
  sracipeConfig(rsSRacipe)$stochParams["initialNoise"] <- 50
  sracipeConfig(rsSRacipe)$stochParams["noiseScalingFactor"] <- 0.5
}
    else{
      rsSRacipe <- rsSRsetAnneal
    }
  output$CN <- renderText("")
  output$Anneal <- renderText("")

#  observeEvent(input$sRacipeOption,{

isolate(
    if(input$sRacipeOption == "constantNoise")
      {
      output$CN <- renderText("Constant Noise Plots")
      sracipeConfig(rsSRacipe)$stochParams["nNoise"] <- 2
      sracipeConfig(rsSRacipe)$stochParams["initialNoise"] <- 50*(0.5)^(20-input$sRacipeNoise)
      withProgress(message = 'Simulating', value = 0.25, {

      rsSRacipe <- simulateRS(rsSRacipe)
      sRacipeSet <<- rsSRacipe
})
      withProgress(message = 'Plotting', value = 0.25, {

      simulationDataAll <- assay(rsSRacipe)
      geneNames <- varMetadata(rsSRacipe@featureData)$labelDescription
      nGenes <- length(geneNames)
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-1)+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"])
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]

      means <- colMeans(simulationData)
      sds <- apply(simulationData, 2, sd)
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")

      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames


      pca = prcomp(simulationData, scale. = FALSE, center = FALSE)
      #pcaData <- data.frame(x=pca$x[,1],y=pca$x[,2])
      rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
      plotColor <- rf(32)
      binCount <- 40


      i=1
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i)+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i+1)
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")
      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames
      pcaData <- scale(simulationData, pca$center, pca$scale) %*% pca$rotation
      pcaData <- data.frame(x=pcaData[,1],y=pcaData[,2])

      i=2
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i)+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i+1)
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")
      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames

      output$sRacipePca <-renderPlot({


        pcaData <- scale(simulationData, pca$center, pca$scale) %*% pca$rotation
        pcaData <- data.frame(x=pcaData[,1],y=pcaData[,2])
        p2 <- plotDensity(pcaData,binCount,plotColor)

      })
      output$sRacipeHeatmap <- renderPlot({
        if(input$simulateSRacipe == 0) return()
        .sracipePlotHeatmap(simulationData)
      })
})

    }
)

if(input$sRacipeOption == "annealing")
{
      output$Anneal <- renderText("Annealing Simulation Data")

      sracipeConfig(rsSRacipe)$stochParams["nNoise"] <- 20
      sracipeConfig(rsSRacipe)$stochParams["initialNoise"] <- 50
      sracipeConfig(rsSRacipe)$stochParams["noiseScalingFactor"] <- 0.5
      if(!annealFlag){
        withProgress(message = 'Simulating', value = 0.25, {

      rsSRacipe <- simulateRS(rsSRacipe, annealing = TRUE)
      annealFlag <<- TRUE
      rsSRsetAnneal <<- rsSRacipe
      sRacipeSet <<- rsSRacipe
        })
      }


      withProgress(message = 'Plotting', value = 0.25, {

      simulationDataAll <- assay(rsSRacipe)
      geneNames <- varMetadata(rsSRacipe@featureData)$labelDescription
      nGenes <- length(geneNames)
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-1)+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"])
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]

      means <- colMeans(simulationData)
      sds <- apply(simulationData, 2, sd)
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")

      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames


      pca = prcomp(simulationData, scale. = FALSE, center = FALSE)
      #pcaData <- data.frame(x=pca$x[,1],y=pca$x[,2])
      rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
      plotColor <- rf(32)
      binCount <- 40


      i=1
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i)+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-i+1)
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")
      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames
      pcaData <- scale(simulationData, pca$center, pca$scale) %*% pca$rotation
      pcaData <- data.frame(x=pcaData[,1],y=pcaData[,2])


      tmpNoise <- reactive({input$sRacipeNoise})
      col_start <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-tmpNoise() )+1
      col_end <- nGenes*(sracipeConfig(rsSRacipe)$stochParams["nNoise"]-tmpNoise() +1)
      simulationData <- as.data.frame(simulationDataAll[,col_start:col_end])
      simulationData <- log2(simulationData)
      simulationData <- simulationData[is.finite(rowMeans(simulationData)), ]
      simulationData <- sweep(simulationData,2,means,FUN = "-")
      simulationData <- sweep(simulationData,2,sds,FUN = "/")
      modelNames <- seq(1:nrow(simulationData))
      row.names(simulationData) <- modelNames
      colnames(simulationData) <- geneNames

      output$sRacipePca <-renderPlot({
        pcaData <- scale(simulationData, pca$center, pca$scale) %*% pca$rotation
        pcaData <- data.frame(x=pcaData[,1],y=pcaData[,2])
        p2 <- .sracipePlotDensity(pcaData,binCount,plotColor)
      })

      output$sRacipeHeatmap <- renderPlot({
        if(input$simulateSRacipe == 0) return()
        .sracipePlotHeatmap(simulationData)
      })

})

}

})

  output$downloadDataSRacipe <- downloadHandler(
    filename = function(){
      if(input$downloadDataSRacipeType == "txt"){
        return(paste0(annotation(sRacipeSet),".zip"))}
      else return(paste0(annotation(sRacipeSet),".RDS"))
    },

    content = function(file){
      if(input$downloadDataSRacipeType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        fileName <- paste(annotation(sRacipeSet),"_GE",".txt",sep = "")
        write.table(assay(sRacipeSet),fileName,sep = ' ', row.names = TRUE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        # fileName <- paste(annotation(sRacipeSet),"_ic",".csv",sep = "")
        # write.table(ic(sRacipeSet),fileName,sep = ',', row.names = F, col.names = T)
        # files <- c(fileName,files)

        fileName <- paste(annotation(sRacipeSet),"_network",".txt",sep = "")
        write.table(sracipeCircuit(sRacipeSet),fileName,sep = ' ', row.names = FALSE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(annotation(sRacipeSet),"_params",".txt",sep = "")
        write.table(sracipeParams(sRacipeSet),fileName,sep = ' ', row.names = TRUE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)
      }
      else{

        saveRDS(sRacipeSet, file)

      }


    })

  observeEvent(input$saveDataSRacipe,{
    show("uploadDataSRacipe")
    show("uploadToDatabaseUI_name_sRacipe")
    show("uploadToDatabaseUI_lab_sRacipe")
    show("uploadToDatabaseUI_contact_sRacipe")
    show("uploadToDatabaseUI_title_sRacipe")
    show("uploadToDatabaseUI_abstract_sRacipe")
    show("uploadToDatabaseUI_url_sRacipe")
    show("uploadToDatabaseUI_pubMedIds_sRacipe")

  })

  observeEvent(input$uploadDataSRacipe,{
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_sRacipe
    tmp@lab <- input$uploadToDatabaseUI_lab_sRacipe
    tmp@contact <- input$uploadToDatabaseUI_contact_sRacipe
    tmp@title <- input$uploadToDatabaseUI_title_sRacipe
    tmp@abstract <- input$uploadToDatabaseUI_abstract_sRacipe
    tmp@url <- input$uploadToDatabaseUI_url_sRacipe
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_sRacipe

    experimentData(sRacipeSet) <- tmp
    saveRDS(sRacipeSet, file = paste0("usrDatabase/",annotation(sRacipeSet),"_sRACIPE.RDS"))
    output$fileDataSRacipe <- renderText(HTML("File uploaded to Database"))
    show("fileDataSRacipe")
  })


})


