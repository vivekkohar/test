###########################################
# Validate Server
###########################################
validateVars = reactiveValues()
validateVars$simExp <- NULL
validateVars$refClust <- NULL


# Update the network if [Load Circuit] button is clicked from [Load File] tab
observeEvent(input$fileRefExp,{
  refExp <-  input$fileRefExp
  if(is.null(refExp)) {
    return(NULL)
  }
  refExp <- as.matrix(read.table(
    refExp$datapath, sep = " ", header = TRUE))
#  rownames(refExp) <- refExp[,1]
#  refExp <- as.matrix(refExp[-1,])
  validateVars$refExp <- reactive(refExp)
  print(validateVars$refExp()[1:2,1:2])
   # rownames(uploadedData) <- uploadedData[,1]
   # uploadedData <- uploadedData[,2:dim(uploadedData)[2]]
  # flush the outputs if new data is uploaded

})

observeEvent(input$fileSimExp,{
  simExp <-  input$fileSimExp
  if(is.null(simExp)) {
    return(NULL)
  }
  simExp <- as.matrix(read.table(
    simExp$datapath, sep = " ", header = TRUE))
  validateVars$simExp <- reactive(simExp)
  print(validateVars$simExp()[1:2,1:2])
})

  # Update the network if [Load Circuit] button is clicked from [Load File] tab
  observeEvent(input$uploadRefClust,{
    refClust <-  input$fileRefClust
    if(is.null(refClust)) {
      return(NULL)
    }
    refClust <- as.character(read.table(refExp$datapath,sep="", header =FALSE,
                                        stringsAsFactors = FALSE))
   validateVars$refClust <- reactive(refClust)
 })


  observeEvent(input$compareValidate,{
    withBusyIndicatorServer("compareValidate",{
    validateVars$pValue <- reactive(isolate(input$validatePValue))
    validateVars$nClust <- reactive(isolate(input$validateNClust))
    validateVars$validateNPermut <- reactive(isolate(input$stepSizeExplorer ))
 #   print(validateVars$pValue)
#    print(validateVars$nClust)
#    print(validateVars$validateNPermut)

    refExp <-  as.matrix(validateVars$refExp())
    simExp <- as.matrix(validateVars$simExp())
   print(refExp[1:2,1:2])
   print(simExp[1:2,1:2])
  similarity <- sracipeHeatmapSimilarity(refExp,simExp, 
                                  returnData = TRUE,
                        #         clusterCut = isolate(validateVars$refClust()),
                                  pValue = (input$validatePValue),
                                  nClusters = (input$validateNClust),
                                  permutations = (input$validateNPermut))
  
  plotData <- data.frame(t(similarity$simulated.refCor))
  plotData$row <- as.character(seq(1:ncol(similarity$simulated.refCor)))
  plotData <- reshape2::melt(plotData, value.name = "correlation")
  plotData$refName <- rep(colnames(similarity$simulated.refCor),
                          nrow(similarity$simulated.refCor))
  plotData$simName <- rep(rownames(similarity$simulated.refCor),
                          ncol(similarity$simulated.refCor))
  # image(similarity$simulated.refCor, col = plotColor, 
  # xlab = "Simulation Data Samples", ylab = "Ref Data Samples", axes=FALSE)
  ggplot(data=plotData,
         aes(x=variable, y=row, fill=correlation)) + geom_tile() + 
    xlab("Simulation Data Samples") +
    ylab("Ref Data Samples") +
    scale_x_discrete(labels= plotData$simName) +
    scale_y_discrete(labels= plotData$refName) +
    #geom_text(aes(label=correlation), color='white') + 
    theme_bw()
  
  output$validateRefHeatmap <- renderPlot({
    if(is.null(validateVars$refExp())) return()
    plotData <- validateVars$refExp()
    gplots::heatmap.2(similarity$dataReference, trace = "none",
                      dendrogram = "none", Colv=FALSE, col = plotColor,
                      main = "Reference Data")
    
  
  })
  
  output$validateSimHeatmap <- renderPlot({
    if(is.null(validateVars$simExp())) return()
    gplots::heatmap.2(similarity$dataSimulation, trace = "none", 
                      dendrogram = "none", Colv=FALSE, col = plotColor,
                      main = "Simulated Data")
  })
  output$validateRefSim <- renderPlot({
    if(is.null(similarity)) return()
    # refClusters <- integer()
    # clusters <- similarity$ref.cluster.freq[-1]
    # for(i in seq_len(dim(clusters)[1])){
    #   refClusters <- c(refClusters,
    #                    rep(i,(clusters[i]*ncol(similarity$simulated.refCor))))
    # }
    # clusters <- similarity$simulated.cluster.freq[-1]
    # simClusters <- integer()
    # for(i in seq_len(dim(clusters)[1])){
    #   simClusters <- c(simClusters,
    #                    rep(i,(clusters[i]*nrow(similarity$simulated.refCor))))
    # }
    # simClusters <- c(simClusters,rep(dim(similarity$simulated.refCor)[1],
    #                                  as.integer(similarity$simulated.cluster.freq[1]*
    #                                    nrow(similarity$simulated.refCor))))
    # refClusters <- c(refClusters,rep(dim(similarity$simulated.refCor)[1], 
    #                                  similarity$ref.cluster.freq[1]*
    #                                    nrow(similarity$simulated.refCor)))
    simClusters <- as.character(col2[(1+similarity$simClusters)])
    refClusters <- as.character(col2[(1+similarity$refClusters)])
    # print((simClusters))
    # print((refClusters))
    # print(length(simClusters))
    # print(length(refClusters))
    # print(dim(similarity$simulated.refCor))
    #print(refClusters)
    #print(simClusters)
    gplots::heatmap.2(as.matrix(similarity$simulated.refCor), Rowv = FALSE,
                      Colv = FALSE, ColSideColors = refClusters,
                      RowSideColors = simClusters ,trace = "none",
                      ylab =  "Simulation Data Samples",
                      xlab = "Ref Data Samples", col = plotColor, 
                      dendrogram = 'none',
main = "Correlation between reference
          and simulated samples" )
    # image(similarity$simulated.refCor, col = plotColor, 
    #       xlab = "Simulation Data Samples", ylab = "Ref Data Samples", 
    #       axes=FALSE, useRaster = TRUE, title = "Correlation between reference
    #       and simulated samples")
  })
  
  output$validateRefClustTable <- renderTable({ if(is.null(similarity)) return()
    tableData <- data.frame(similarity$ref.cluster.freq*100)
    colnames(tableData) <- c("Cluster", "Percentage in Reference Data")
    tableData <- tableData[-1,]
    return(tableData)
    })
  output$validateSimClustTable <- renderTable({if(is.null(similarity)) return()
    tableData <- data.frame(similarity$simulated.cluster.freq*100)
    colnames(tableData) <- c("Cluster", "Percentage in Simulated Data")
    tableData <- tableData[-1,]
    return(tableData)
    })
  # 
  # output$validateClustTable <- renderTable({if(is.null(similarity)) return()
  #   tableData <- similarity$cluster.similarity
  #   colnames(tableData) <- c("Cluster", "Percentage in Reference Data")
  #   return(tableData)
  #   })
  output$validateKL <- renderText({
    return(paste("KL distance between the simulated and reference data: ", 
                 similarity$KL,"."))
    
    })
    })
  
  })
  
