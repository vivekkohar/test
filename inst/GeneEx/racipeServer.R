
###########################################
# RACIPE
###########################################

rSetRacipe <- RacipeSE()
racipeVals <- reactiveValues()
sracipeCircuit(rSetRacipe) <- demoCircuit
racipeVals$rSet <- rSetRacipe
# Use a flag 
racipeVals$annealFlag <- FALSE


output$racipeCircuit <- renderVisNetwork({
  sracipePlotCircuit(racipeVals$rSet, plotToFile = FALSE)
})



observeEvent(input$simulateRacipe, {
  withBusyIndicatorServer("simulateRacipe",{
  shinyjs::show("racipeDeterministicText")
  shinyjs::show("racipeHeatmap")
  shinyjs::show("racipePca")
  shinyjs::show("parametricAnalysisRacipe")
  shinyjs::show("stochasticRacipe")
  shinyjs::show("paDescrpt")
  shinyjs::show("downloadRacipeData")
  shinyjs::show("downloadRacipeDataType")
  shinyjs::show("saveRacipeData")

  shinyjs::show("validateRacipe")

  rsRacipe <- racipeVals$rSet
#  annotation(rsRacipe) <- paste0(Sys.Date(),"_",input$filenameTopo)
#  sracipeCircuit(rsRacipe) <- isolate(racipeVals$shinyRacipeNetwork())
  sracipeConfig(rsRacipe)$simParams["integrateStepSize"] <- 
    isolate(input$stepSizeRacipe)
  sracipeConfig(rsRacipe)$simParams["simulationTime"] <-  
    isolate(input$simTimeRacipe)
  sracipeConfig(rsRacipe)$stochParams["numModels"] <-
    isolate(input$numModels)
  sracipeConfig(rsRacipe)$stochParams["paramRange"] <-  
    isolate(input$parameterRange)
 # anneal <- switch(input$sRacipeOption,
#                   constantNoise = FALSE,
#                   annealing = TRUE,
#                   FALSE)

  #      rs <- shinyModelExplorer()
 # withProgress(message = 'Simulating', value = 0.25, {
    # shinyBS::createAlert(session, anchorId = "racipeAlert",
    #                      alertId =  "racipeProcessing", title = "Processing",
    #                      content = "Please wait...", append = FALSE)
  rsRacipe <- sracipeSimulate(rsRacipe, plots = FALSE,genIC = TRUE, 
                              genParams = TRUE, integrate = TRUE, 
                              numModels = isolate(input$numModels),
                              integrateStepSize = isolate(input$stepSizeRacipe),
                              simulationTime = isolate(input$simTimeRacipe),
                              paramRange = isolate(input$parameterRange)
                              )
  rsRacipe <- sracipeNormalize(rsRacipe)
  racipeVals$rsRacipe <- reactive(rsRacipe)
  # if(input$emailRacipe){
  #   zipFile <- paste0(tempfile(),annotation(racipeVals$rsRacipe()),".zip")
  #   files <- NULL;
  #   
  #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_GE",".txt",
  #                     sep = "")
  #   write.table(assay(racipeVals$rsRacipe()),fileName,sep = ' ', 
  #               row.names = TRUE, 
  #               col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  #   
  #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_IC",".txt",
  #                     sep = "")
  #   write.table(sracipeIC(racipeVals$rsRacipe()),fileName,sep = ' ',
  #               row.names = FALSE, col.names = TRUE)
  #   files <- c(fileName,files)
  #   
  #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_network",".txt",
  #                     sep = "")
  #   write.table(sracipeCircuit(racipeVals$rsRacipe()),fileName,sep = ' ', 
  #               row.names = FALSE, col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  #   
  #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_params",".txt",
  #                     sep = "")
  #   write.table(sracipeParams(racipeVals$rsRacipe()),fileName,sep = ' ', 
  #               row.names = TRUE, col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  #   
  #   #create the zip file
  #   zip(zipFile,files)
  #   fileName <- paste0(annotation(racipeVals$rsRacipe()),"_Racipe")
  #   googledrive::drive_upload(zipFile,path = "download",name =fileName, overwrite = TRUE )
  #   googledrive::drive_share(fileName,role = "reader",type = "anyone")
  #   fileLink <- googledrive::drive_link(fileName)
  #   email <-
  #     gm_mime() %>%
  #     gm_to(input$userEmail) %>%
  #     gm_from("geneex.maintainer@gmail.com") %>%
  #     gm_subject("Your GeneEx Simulation Results are available") %>%
  #     gm_text_body(fileLink)
  #   
  #   gm_send_message(email)
  #   
  # }
#  shinyBS::closeAlert(session, alertId = "racipeProcessing")
 # })

  output$racipeDeterministicText <- renderUI({HTML(
    "Hierarchical clustering and principal component 
    analysis of deterministic simulations ")})


  output$racipePca <- renderPlot({
    if(input$simulateRacipe == 0) return()
    withProgress(message = 'Plotting pca', value = 0.25, {
      plotData <- assay(rsRacipe)
      pca = prcomp(t(plotData), scale. = FALSE)
      racipeVals$pca <- pca
      pcaData <- data.frame(x=pca$x[,1],y=pca$x[,2])
        .sracipePlotDensity(pcaData, 
                            label1 = paste0("PC1(",100*summary(pca)$importance[2,1],"%)"),
                            label2 = paste0("PC2(",100*summary(pca)$importance[2,2],"%)"),
                            title = title)

    })
  })
  output$racipeHeatmap <- renderPlot({
    if(input$simulateRacipe == 0) return()
    withProgress(message = 'Hierarchical clustering', value = 0.25, {
      plotData <- assay(rsRacipe)
      .sracipePlotHeatmap(plotData, nClusters = input$racipeNClusters)
    })
  })
  })
  ###########################################
  # Parametric Analysis
  ###########################################
  observeEvent(input$parametricAnalysisRacipe, {
    shinyjs::show("filteredOutputRacipe")
    shinyjs::show("filterSliderRacipe")
    shinyjs::show("filteredOutputRacipe2")
    shinyjs::show("filterSliderRacipe2")
    shinyjs::show("filteredOutputRacipe3")
    shinyjs::show("filterSliderRacipe3")
    
    shinyjs::show("racipeHeatmapFiltered")
    shinyjs::show("racipePcaFiltered")
  if(!is.null(rsRacipe)){
    parameterNames <- sracipeGenParamNames(rsRacipe)
    parameters <- as.matrix(sracipeParams(rsRacipe))
    # print(storage.mode(parameters))
    # print(class(parameters))
  }
    output$filteredOutputRacipe <- renderUI({
      #  if(is.null(parameters)) return(NULL)
      # print(parameterNames)
      selectInput("selectedParameter", "Parameter",
                  parameterNames,
                  selected = parameterNames[1]
      )
    })
    
    output$filteredOutputRacipe2 <- renderUI({
      #  if(is.null(parameters)) return(NULL)
      
      selectInput("selectedParameter2", "Parameter",
                  parameterNames,
                  selected = parameterNames[2]
      )
    })
    
    output$filteredOutputRacipe3 <- renderUI({
      #  if(is.null(parameters)) return(NULL)
      
      selectInput("selectedParameter3", "Parameter",
                  parameterNames,
                  selected = parameterNames[3]
      )
    })
    
  dataSimulation <- data.frame(t(assay(rsRacipe,1))) #sracipeNormalize(rsRacipe)
  pca = prcomp((dataSimulation), scale. = FALSE, center = FALSE)
  filtered <- reactive({
    if (is.null(dataSimulation)) {
      return(NULL)
    }
    dataSimulation[((parameters[,input$selectedParameter] >= 
0.01*(input$parameterInput[1]*max(parameters[,input$selectedParameter]))) & 
  (parameters[,input$selectedParameter] < 
     0.01*(input$parameterInput[2]*max(parameters[,input$selectedParameter]))) &
    (parameters[,input$selectedParameter2] >= 0.01*(input$parameterInput2[1]*
    max(parameters[,input$selectedParameter2]))) & 
  (parameters[,input$selectedParameter2] < 
  0.01*(input$parameterInput2[2]*max(parameters[,input$selectedParameter2]))) &
  (parameters[,input$selectedParameter3] >= 0.01*(input$parameterInput3[1]*
  max(parameters[,input$selectedParameter3]))) & 
  (parameters[,input$selectedParameter3] < 0.01*(input$parameterInput3[2]*
                                max(parameters[,input$selectedParameter3])))
                    ),]

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
    pcaData <- filtered()
    pcaData <- scale(pcaData, pca$center, pca$scale) %*% pca$rotation
    pcaData <- data.frame(PC1=pcaData[,1],PC2=pcaData[,2])
    .sracipePlotDensity(pcaData)
  })

  output$racipeHeatmapFiltered <- renderPlot({
    if(is.null(filtered())) return(NULL)
    withProgress(message = 'Hierarchichal clustering', value = 0.25, {
      plotData <- filtered()
      .sracipePlotHeatmap(t(plotData),nClusters = input$racipeNClusters)
  })
  })

})
 observeEvent(input$validateRacipe,{
   validateVars$simExp <- reactive(assay(racipeVals$rsRacipe(),1))
   output$validateSimHeatmap <- renderPlot({     return()})
   output$validateRefHeatmap <- renderPlot({     return()})
   output$validateRefSim <- renderPlot({     return()})
   output$validateRefClustTable <- renderTable({  return() })
   output$validateSimClustTable <- renderTable({return()})
   output$validateKL <- renderText({return()})
   output$validateSimHeatmap <- renderPlot({
     if(is.null(validateVars$simExp())) return()
     gplots::heatmap.2(validateVars$simExp, trace = "none", 
                       Colv=TRUE, col = plotColor,
                       main = "Simulated Data")
   })
})

  ########################### Download and Upload ########################
  output$downloadRacipeData <- downloadHandler(
    filename = function(){
      if(input$downloadRacipeDataType == "txt"){
        return(paste0(Sys.time(),"_",annotation(racipeVals$rsRacipe()),".zip"))}
      else return(paste0(Sys.time(),"_",annotation(racipeVals$rsRacipe()),".RDS"))
    },

    content = function(file){
      if(input$downloadRacipeDataType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_GE",".txt",
                          sep = "")
        write.table(assay(racipeVals$rsRacipe()),fileName,sep = ' ', 
                    row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_IC",".txt",
                          sep = "")
        write.table(sracipeIC(racipeVals$rsRacipe()),fileName,sep = ' ',
                    row.names = FALSE, col.names = TRUE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_network",".txt",
                          sep = "")
        write.table(sracipeCircuit(racipeVals$rsRacipe()),fileName,sep = ' ', 
                    row.names = FALSE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_params",".txt",
                          sep = "")
        write.table(sracipeParams(racipeVals$rsRacipe()),fileName,sep = ' ', 
                    row.names = TRUE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)
      }
      else{

        saveRDS(racipeVals$rsRacipe(), file)

      }


    })

  observeEvent(input$saveRacipeData,{
    shinyjs::show("uploadRacipeData")
    shinyjs::show("uploadToDatabaseUI_name_Racipe")
    shinyjs::show("uploadToDatabaseUI_lab_Racipe")
    shinyjs::show("uploadToDatabaseUI_contact_Racipe")
    shinyjs::show("uploadToDatabaseUI_title_Racipe")
    shinyjs::show("uploadToDatabaseUI_abstract_Racipe")
    shinyjs::show("uploadToDatabaseUI_url_Racipe")
    shinyjs::show("uploadToDatabaseUI_pubMedIds_Racipe")
    shinyjs::hide("fileRacipeData")

  })

  observeEvent(input$uploadRacipeData,{
    withBusyIndicatorServer("uploadRacipeData",{
      
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_Racipe
    tmp@lab <- input$uploadToDatabaseUI_lab_Racipe
    tmp@contact <- input$uploadToDatabaseUI_contact_Racipe
    tmp@title <- input$uploadToDatabaseUI_title_Racipe
    tmp@abstract <- input$uploadToDatabaseUI_abstract_Racipe
    tmp@url <- input$uploadToDatabaseUI_url_Racipe
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_Racipe

    metadata(rsRacipe)$meta <- tmp
  #  saveRDS(rsRacipe, file = paste0("usrDatabase/",
  #                                  Sys.time(),"_",annotation(rsRacipe),"_RACIPE.RDS"))
    filename <- paste(tempdir(),"/",annotation(racipeVals$rsRacipe()),"_Racipe.RDS", sep = "")
    saveRDS(rsRacipe, file = filename)
    googledrive::drive_upload(
      filename,path = "upload",overwrite = TRUE,
      name = paste(annotation(racipeVals$rsRacipe()),"_Racipe.RDS", sep = ""))
    
    output$fileRacipeData <- renderText(HTML("File uploaded to Database"))
    shinyjs::show("fileRacipeData")
    })
    shinyjs::hide("uploadRacipeData")
    shinyjs::hide("uploadToDatabaseUI_name_Racipe")
    shinyjs::hide("uploadToDatabaseUI_lab_Racipe")
    shinyjs::hide("uploadToDatabaseUI_contact_Racipe")
    shinyjs::hide("uploadToDatabaseUI_title_Racipe")
    shinyjs::hide("uploadToDatabaseUI_abstract_Racipe")
    shinyjs::hide("uploadToDatabaseUI_url_Racipe")
    shinyjs::hide("uploadToDatabaseUI_pubMedIds_Racipe")
  })
}) 
###########################################
# sRACIPE
###########################################
observeEvent(input$stochasticRacipe, {
  
  shinyjs::show("sRacipeOption")
  shinyjs::show("sRacipeNoise")
  shinyjs::show("simulateSRacipe")
  shinyjs::show("emailSRacipe")
  shinyjs::show("sRacipeHeatmap")
  shinyjs::show("sRacipeHeatmapDet")
  shinyjs::show("sRacipePca")
  shinyjs::show("sRacipePcaDet")
  shinyjs::show("saveSRacipeData")
  shinyjs::show("downloadSRacipeData")
  shinyjs::show("downloadSRacipeDataType")

  observeEvent(input$simulateSRacipe, {
    withBusyIndicatorServer("simulateSRacipe",{
#  show("sRacipeHeatmap")
#  show("sRacipePca")
# if(!racipeVals$annealFlag){
#   
# 
#   sracipeConfig(rsSRacipe)$simParams["integrateStepSize"] <-
    # input$stepSizeRacipe
#   sracipeConfig(rsSRacipe)$simParams["simulationTime"] <-
    # input$simTimeRacipe
#   sracipeConfig(rsSRacipe)$stochParams["numModels"] <- input$numModels
#   sracipeConfig(rsSRacipe)$stochParams["paramRange"] <-   input$parameterRange
# 
#   sracipeConfig(rsSRacipe)$stochParams["nNoise"] <- 20
#   sracipeConfig(rsSRacipe)$stochParams["initialNoise"] <- 50
#   sracipeConfig(rsSRacipe)$stochParams["noiseScalingFactor"] <- 0.5
# }
#     else{
#       rsSRacipe <- rsSRsetAnneal
#     }
  output$CN <- renderText("")
  output$Anneal <- renderText("")

#  observeEvent(input$sRacipeOption,{

isolate(
    if(input$sRacipeOption == "constantNoise")
      {
      output$CN <- renderText("Constant Noise Plots")
      rsSRacipe <- racipeVals$rsRacipe()
      detParam <- FALSE
      if(is.null(sracipeParams(rsSRacipe))) detParam <- TRUE
      withProgress(message = 'Simulating', value = 0.25, {
        shinyBS::createAlert(session, anchorId = "racipeAlert",
                          alertId =  "racipeProcessing", title = "Processing",
                             content = "Please wait...", append = FALSE)

      rsSRacipe <- sracipeSimulate(
        rsSRacipe, plots = FALSE, anneal = FALSE,
        nNoise = 1, numModels = isolate(input$numModels),
        integrateStepSize = isolate(input$stepSizeRacipe),
        simulationTime = isolate(input$simTimeRacipe),
        paramRange = isolate(input$parameterRange),
        initialNoise =  50*(0.5)^(20-isolate(input$sRacipeNoise)),
        genParams = detParam, genIC = detParam)
      rsSRacipe <- sracipeNormalize(rsSRacipe)
      
      racipeVals$rsSRacipe <- reactive(rsSRacipe)
      # if(input$emailSRacipe){
      #   zipFile <- paste0(tempfile(),annotation(racipeVals$rsSRacipe()),".zip")
      #   files <- NULL;
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipe()),"_GE",".txt",
      #                     sep = "")
      #   write.table(assays(racipeVals$rsSRacipe()),fileName,sep = ' ', 
      #               row.names = TRUE, 
      #               col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipe()),"_IC",".txt",
      #                     sep = "")
      #   write.table(sracipeIC(racipeVals$rsSRacipe()),fileName,sep = ' ',
      #               row.names = FALSE, col.names = TRUE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipe()),"_network",".txt",
      #                     sep = "")
      #   write.table(sracipeCircuit(racipeVals$rsSRacipe()),fileName,sep = ' ', 
      #               row.names = FALSE, col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipe()),"_params",".txt",
      #                     sep = "")
      #   write.table(sracipeParams(racipeVals$rsSRacipe()),fileName,sep = ' ', 
      #               row.names = TRUE, col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   #create the zip file
      #   zip(zipFile,files)
      #   fileName <- paste0(annotation(racipeVals$rsRacipe()),"_CN")
      #   googledrive::drive_upload(zipFile,path = "download",name =fileName, overwrite = TRUE )
      #   googledrive::drive_share(fileName,role = "reader",type = "anyone")
      #   fileLink <- googledrive::drive_link(fileName)
      #   email <-
      #     gm_mime() %>%
      #     gm_to(input$userEmail) %>%
      #     gm_from("geneex.maintainer@gmail.com") %>%
      #     gm_subject("Your GeneEx Simulation Results are available") %>%
      #     gm_text_body(fileLink)
      #   
      #   gm_send_message(email)
      #   
      # }
})
#      withProgress(message = 'Plotting', value = 0.25, {

        assayDataTmp <- assays(rsSRacipe)
        metadataTmp <- metadata(rsSRacipe)
        plotData <- assayDataTmp[[1]]
        pca1 = prcomp(t(plotData), scale. = FALSE) 
        #summary(prcomp(plotData, scale. = FALSE))
        
      output$sRacipePcaDet <-renderPlot({
        if(is.null(plotData)) return(NULL)
        .sracipePlotPca(plotData = plotData, pca = pca1,
                        title = "Deterministic")
        })
      
      output$sRacipeHeatmapDet <- renderPlot({
        if(input$simulateSRacipe == 0) return()
        .sracipePlotHeatmap(plotData, nClusters = input$racipeNClusters)
      })
      plotDataSt <- assayDataTmp[[2]]
      # plotDataSt <- plotDataSt[is.finite(rowMeans(plotDataSt)),]
      
      stochasticPca <- (scale(t(plotDataSt), pca1$center, pca1$scale) %*%
                           pca1$rotation)
      
      
      output$sRacipePca <-renderPlot({
        if(is.null(plotDataSt)) return(NULL)
        
        pcaData <- data.frame(x=stochasticPca[,1],y=stochasticPca[,2])

        pca1$x <- stochasticPca
        .sracipePlotPca(plotData = stochasticPca, pca = pca1,
                        title = "Stochastic" )
      })
      output$sRacipeHeatmap <- renderPlot({
        if(input$simulateSRacipe == 0) return()
        .sracipePlotHeatmap(plotDataSt, nClusters = input$racipeNClusters)
      })
      shinyBS::closeAlert(session, alertId = "racipeProcessing")
#})

    }
)

if(input$sRacipeOption == "annealing")
{
      output$Anneal <- renderText("Annealing Simulation Data")

      if(!racipeVals$annealFlag){
        rsSRacipeAnneal <- racipeVals$rsRacipe()
        detParam <- FALSE
        if(is.null(sracipeParams(rsSRacipeAnneal))) detParam <- TRUE
        withProgress(message = 'Simulating', value = 0.25, {
          shinyBS::createAlert(
            session, anchorId = "racipeAlert",
            alertId =  "racipeProcessing", title = "Processing",
            content = "Please wait...", append = FALSE)
          rsSRacipeAnneal <- sracipeSimulate(
            rsSRacipeAnneal, annealing = TRUE, nNoise = 20, 
            numModels = isolate(input$numModels),
            integrateStepSize = isolate(input$stepSizeRacipe),
            simulationTime = isolate(input$simTimeRacipe),
            paramRange = isolate(input$parameterRange),
            initialNoise =  50/sqrt(length(names(rsSRacipeAnneal))),
                                   noiseScalingFactor = 0.5,
            genIC = detParam, genParams = detParam)
          rsSRacipeAnneal <- sracipeNormalize(rsSRacipeAnneal)
          
      racipeVals$annealFlag <- TRUE
      racipeVals$rsSRacipeAnneal <- reactive(rsSRacipeAnneal)
      # if(input$emailSRacipe){
      #   zipFile <- paste0(tempfile(),annotation(racipeVals$rsSRacipeAnneal()),".zip")
      #   files <- NULL;
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipeAnneal()),"_GE",".txt",
      #                     sep = "")
      #   write.table(assays(racipeVals$rsSRacipeAnneal()),fileName,sep = ' ', 
      #               row.names = TRUE, 
      #               col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipeAnneal()),"_IC",".txt",
      #                     sep = "")
      #   write.table(sracipeIC(racipeVals$rsSRacipeAnneal()),fileName,sep = ' ',
      #               row.names = FALSE, col.names = TRUE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipeAnneal()),"_network",".txt",
      #                     sep = "")
      #   write.table(sracipeCircuit(racipeVals$rsSRacipeAnneal()),fileName,sep = ' ', 
      #               row.names = FALSE, col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   fileName <- paste(tempdir(),"/",annotation(racipeVals$rsSRacipeAnneal()),"_params",".txt",
      #                     sep = "")
      #   write.table(sracipeParams(racipeVals$rsSRacipeAnneal()),fileName,sep = ' ', 
      #               row.names = TRUE, col.names = TRUE, quote = FALSE)
      #   files <- c(fileName,files)
      #   
      #   #create the zip file
      #   zip(zipFile,files)
      #   fileName <- paste0(annotation(racipeVals$rsRacipe()),"_Anneal")
      #   googledrive::drive_upload(zipFile,path = "download",name =fileName, 
      #                             overwrite = TRUE )
      #   googledrive::drive_share(fileName,role = "reader",type = "anyone")
      #   fileLink <- googledrive::drive_link(fileName)
      #   email <-
      #     gm_mime() %>%
      #     gm_to(input$userEmail) %>%
      #     gm_from("geneex.maintainer@gmail.com") %>%
      #     gm_subject("Your GeneEx Simulation Results are available") %>%
      #     gm_text_body(fileLink)
      #   
      #   gm_send_message(email)
      #   
      # }
      shinyBS::closeAlert(session, alertId = "racipeProcessing")
        })
      }

 #     withProgress(message = 'Plotting', value = 0.25, {
        rsSRacipeAnneal <- isolate(racipeVals$rsSRacipeAnneal())
        assayDataTmp <- assays(rsSRacipeAnneal)
        metadataTmp <- metadata(rsSRacipeAnneal)
        plotData <- assayDataTmp[[1]]
        pca1 = prcomp(t(plotData), scale. = FALSE)
        #summary(prcomp(plotData, scale. = FALSE))
        
        output$sRacipePcaDet <-renderPlot({
          if(is.null(plotData)) return(NULL)
          .sracipePlotPca(plotData = plotData, pca = pca1, 
                          title = "Deterministic")
        })
        
        output$sRacipeHeatmapDet <- renderPlot({
          if(input$simulateSRacipe == 0) return()
          .sracipePlotHeatmap(plotData, nClusters = input$racipeNClusters)
        })
        plotDataSt <- assayDataTmp[[(22 - input$sRacipeNoise)]]
        # plotDataSt <- plotDataSt[is.finite(rowMeans(plotDataSt)),]
        
        stochasticPca <- (scale(t(plotDataSt), pca1$center, pca1$scale) %*%
                            pca1$rotation)
        
        
        output$sRacipePca <-renderPlot({
          if(is.null(plotDataSt)) return(NULL)
          
          pcaData <- data.frame(x=stochasticPca[,1],y=stochasticPca[,2])
          pca1$x <- stochasticPca
          .sracipePlotPca(plotData = stochasticPca, pca = pca1,
                          title = "Stochastic")
        })
        output$sRacipeHeatmap <- renderPlot({
          if(input$simulateSRacipe == 0) return()
          .sracipePlotHeatmap(plotDataSt, nClusters = input$racipeNClusters)
        })
        shinyBS::closeAlert(session, alertId = "racipeProcessing")
  #    })

}
})
})

  output$downloadSRacipeData <- downloadHandler(
    filename = function(){
      if(input$downloadSRacipeDataType == "txt"){
        return(paste0(Sys.time(),"_",annotation(racipeVals$rSet),".zip"))}
      else return(paste0(Sys.time(),"_",annotation(racipeVals$rSet),".RDS"))
    },

    content = function(file){
      if(input$downloadSRacipeDataType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL
#        tmpRSet <- NULL
        # print("Here1") 
        tmpRSet <- RacipeSE()
        if(input$sRacipeOption == "annealing")
          tmpRSet <- racipeVals$rsSRacipeAnneal()
        if(input$sRacipeOption == "constantNoise")
        {
          # print("Here") 
          tmpRSet <- racipeVals$rsSRacipe()}
        fileName <- paste(tempdir(),"/",annotation(tmpRSet),"_GE",".txt",sep = "")
        write.table(assays(tmpRSet),fileName,sep = ' ', row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(tmpRSet),"_IC",".txt",sep = "")
        write.table(sracipeIC(tmpRSet),fileName,sep = ' ', row.names = FALSE,
                    col.names = TRUE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(tmpRSet),"_network",".txt",sep = "")
        write.table(sracipeCircuit(tmpRSet),fileName,sep = ' ', 
                    row.names = FALSE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(tempdir(),"/",annotation(tmpRSet),"_params",".txt",sep = "")
        write.table(sracipeParams(tmpRSet),fileName,sep = ' ', 
                    row.names = TRUE, col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)
      }
      else{
        tmpRSet <- RacipeSE()
        if(input$sRacipeOption == "annealing")
          tmpRSet <- racipeVals$rsSRacipeAnneal()
        if(input$sRacipeOption == "constantNoise")
        {
          tmpRSet <- racipeVals$rsSRacipe()}
        saveRDS(tmpRSet, file)

      }


    })

  observeEvent(input$saveSRacipeData,{
    shinyjs::show("uploadSRacipeData")
    shinyjs::show("uploadToDatabaseUI_name_sRacipe")
    shinyjs::show("uploadToDatabaseUI_lab_sRacipe")
    shinyjs::show("uploadToDatabaseUI_contact_sRacipe")
    shinyjs::show("uploadToDatabaseUI_title_sRacipe")
    shinyjs::show("uploadToDatabaseUI_abstract_sRacipe")
    shinyjs::show("uploadToDatabaseUI_url_sRacipe")
    shinyjs::show("uploadToDatabaseUI_pubMedIds_sRacipe")

  })

  observeEvent(input$uploadSRacipeData,{
    withBusyIndicatorServer("uploadSRacipeData",{
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_sRacipe
    tmp@lab <- input$uploadToDatabaseUI_lab_sRacipe
    tmp@contact <- input$uploadToDatabaseUI_contact_sRacipe
    tmp@title <- input$uploadToDatabaseUI_title_sRacipe
    tmp@abstract <- input$uploadToDatabaseUI_abstract_sRacipe
    tmp@url <- input$uploadToDatabaseUI_url_sRacipe
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_sRacipe
    tmpRSet <- NULL
    if(input$sRacipeOption == "annealing")
      tmpRSet <- racipeVals$rsSRacipeAnneal()
    if(input$sRacipeOption == "constantNoise")
      tmpRSet <- racipeVals$rsSRacipe()
    metadata(tmpRSet)$meta <- tmp
    #saveRDS(tmpRSet, file = paste0("usrDatabase/",
    #                               Sys.time(),"_",annotation(tmpRSet),"_sRACIPE.RDS"))
    filename <- paste(tempdir(),"/",annotation(tmpRSet),"_Racipe.RDS", sep = "")
    saveRDS(tmpRSet, file = filename)
    googledrive::drive_upload(
      filename,path = "upload",overwrite = TRUE,
      name = paste(annotation(tmpRSet),"_SRacipe.RDS", sep = ""))
    
    output$fileSRacipeData <- renderText(HTML("File uploaded to Database"))
    shinyjs::show("fileSRacipeData")
    })
    shinyjs::hide("uploadSRacipeData")
    shinyjs::hide("uploadToDatabaseUI_name_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_lab_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_contact_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_title_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_abstract_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_url_sRacipe")
    shinyjs::hide("uploadToDatabaseUI_pubMedIds_sRacipe")
  })


})


