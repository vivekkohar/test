###########################################
# Model Explorer
###########################################

############## Show Hide Options #####################################
observeEvent(input$deterministicSimulations,{

  show("simulateME")

  show("MEts")

})
observeEvent(input$perturbationExplorer,{
  toggle("simTimeExplorer")
  toggle("stepSizeExplorer")
  toggle("modelParams")
  toggle("newModelParamValue")
  toggle("noiseLevel")
  toggle("simulateModifiedME")
  toggle("modifiedMEts")
  toggle("downloadModifiedMEData")
#  toggle("saveModifiedMEData")
})

observeEvent(input$bifurcationExplorer,{
  toggle("modelParamsBif")
  toggle("modelParamBifMin")
  toggle("modelParamBifMax")
  toggle("modelNumBifurs")
  toggle("bifurcationME")
  toggle("modifiedBifME")
  toggle("downloadBifData")
#  toggle("saveBifData")
})

######################### Network Input ######################################

# Array to hold reactive variables like network topology
reactVariables = reactiveValues()
# Limit the maximum number of models to 5000.
reactVariables$modelNumBifurs <- reactive({
  if(input$modelNumBifurs > 5000) return(5000)
  else return(input$modelNumBifurs)
  })

# Limit maximum time to 5000.
reactVariables$simTimeExplorer <- reactive({
  if(input$simTimeExplorer > 5000) return(5000)
  else return(input$simTimeExplorer)
})
# Use maximum and minimum limits on the initial step size
reactVariables$stepSizeExplorer <- reactive({
  if(input$stepSizeExplorer > 1) return(0.99)
  if(input$stepSizeExplorer < .001) return(0.001)
  else return(input$stepSizeExplorer)
})


# Initialize the explorer network to NULL
# Do not use simple reactive as we update the network from different places
reactVariables$explorerNetwork <- reactive({
  if(input$updateTopologyfromText ==0) {
    if(input$updateTopologyfromFile ==0) { return ()}
  }
})

# Update the network if [Load Circuit] button is clicked from [Enter Text] tab
observeEvent(input$updateTopologyfromText,{
  inputText <- isolate(input$uiTopology)
  inputText <- gsub("\t",",", inputText)
  inputText <- gsub("\n",",", inputText)
  inputText <- gsub(" ",",", inputText)

  reactVariables$explorerNetwork <- isolate(
    reactive(read.table(text=inputText,
                        col.names=c('Source','Target', 'Interaction'),
                        sep = ",", stringsAsFactors = FALSE)))

  # flush the outputs if network is reset
  output$MEts <- renderPlot({   return()  })
  output$modifiedMEts <- renderPlot({ return()})
  output$modifiedBifME <- renderPlot({ return()})

})

# Update the network if [Load Circuit] button is clicked from [Load File] tab
observeEvent(input$updateTopologyfromFile,{
  data <-  input$file
  if(is.null(data)) {
    return(NULL)
    }
    reactVariables$explorerNetwork <- reactive( isolate(read.table(
      data$datapath,sep="", header =TRUE, stringsAsFactors = FALSE)))
# update the [Enter Text] with the loaded circuit
      updateTextAreaInput(session, "uiTopology", 
                          value = as.matrix(reactVariables$explorerNetwork()))

      # flush the outputs if network is reset
      output$MEts <- renderPlot({   return()  })
      output$modifiedMEts <- renderPlot({ return()})
      output$modifiedBifME <- renderPlot({ return()})

})

# Message on how to enter the circuit as text
output$networkTextFormat <- renderUI({HTML(
  "Enter the interactions separating the source, target and interactions type by
  space or tab or comma.\n
  Use 1 for activation and 2 for inhibition. For example,\n
  srcA,tgtA,1,srcB,tgtB,2,srcA,tgtB,2")})


output$tb <-renderDT({

  return(reactVariables$explorerNetwork())
}, selection = 'none', editable = FALSE, rownames= FALSE
)



reactVariables$explorerRset <- reactive({
  if(input$updateTopologyfromText ==0) {
    if(input$updateTopologyfromFile ==0) {
      if(input$loadNetworkExplorer == 0)
      { return (NULL)}
    }
  }
})

reactVariables$explorerRset <- reactive({
  rs <-  RacipeSE()
  sracipeCircuit(rs) <- reactVariables$explorerNetwork()
  annotation(rs) <- paste0(Sys.Date(),"_",input$filenameTopo)
  return(rs)
})


output$network <- renderVisNetwork({
  if(input$updateTopologyfromText ==0) {
    if(input$updateTopologyfromFile ==0) {
      if(input$loadNetworkExplorer == 0)
      { return (NULL)}
    }
  }

#if(is.null(reactVariables$explorerRset())) { return (NULL)}
  sracipePlotCircuit(reactVariables$explorerRset(), plotToFile = FALSE)
})

output$downloadCircuit <- downloadHandler(
  filename = function() {
    paste(annotation(reactVariables$explorerRset()), '.tpo', sep='')
  },
  content = function(con) {
    if(is.null(reactVariables$explorerRset())) return(NULL)
    write.table(sracipeCircuit(reactVariables$explorerRset()), con, 
                row.names = FALSE, quote = FALSE)
  }
)

output$downloadSampleNet <- downloadHandler(
  filename = function() {
    paste("demo", '.tpo', sep='')
  },
  content = function(con) {
    demo <- data.frame(Source = c("A", "B"), Target = c("B", "A"), 
                       Interaction = c("2", "2"))
    write.table(demo, con, row.names = FALSE, quote = FALSE)
  }
)


##################### Model Simulations #######################
observeEvent(input$simulateME, {
  show("perturbationExplorer")
  show("bifurcationExplorer")
  show("downloadMEData")
  show("downloadMEDataType")
  show("saveMEData")
  rs <- isolate(reactVariables$explorerRset())
  withProgress(message = 'Simulating', value = 0.25, {
  rs <- sracipeSimulate(rs, timeSeries = TRUE, plots = F)
})
  output$MEts <- renderPlot({

    if(input$simulateME == 0) return()
    withProgress(message = 'Plotting', value = 0.25, {
      plotData <- t(metadata(rs)$timeSeries)
      .sracipePlotTS(plotData)
    })

  })
  # output$info <- renderText({
  #   paste0("x=", input$plot_click_MEts$x, "\ny=", input$plot_click_MEts$y)
  # })

  output$downloadMEData <- downloadHandler(
      filename = function(){
        if(input$downloadMEDataType == "txt"){
        return(paste0(annotation(rs),".zip"))}
        else return(paste0(annotation(rs),".RDS"))
      },

      content = function(file){
        if(input$downloadMEDataType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        fileName <- paste(annotation(rs),"_GE",".txt",sep = "")
        write.table(assays(rs),fileName,sep = ' ', row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        # fileName <- paste(annotation(rs),"_ic",".csv",sep = "")
        # write.table(ic(rs),fileName,sep = ',', row.names = F, col.names = T)
        # files <- c(fileName,files)

        fileName <- paste(annotation(rs),"_network",".txt",sep = "")
        write.table(sracipeCircuit(rs),fileName,sep = ' ', row.names = FALSE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(annotation(rs),"_params",".txt",sep = "")
        write.table(sracipeParams(rs),fileName,sep = ' ', row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)
        }
        else{
            saveRDS(rs, file)
      }
    })

  observeEvent(input$saveMEData,{
    show("uploadMEData")
    show("uploadToDatabaseUI_name_Explorer")
    show("uploadToDatabaseUI_lab_Explorer")
    show("uploadToDatabaseUI_contact_Explorer")
    show("uploadToDatabaseUI_title_Explorer")
    show("uploadToDatabaseUI_abstract_Explorer")
    show("uploadToDatabaseUI_url_Explorer")
    show("uploadToDatabaseUI_pubMedIds_Explorer")
  })

  observeEvent(input$uploadMEData,{
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_Explorer
    tmp@lab <- input$uploadToDatabaseUI_lab_Explorer
    tmp@contact <- input$uploadToDatabaseUI_contact_Explorer
    tmp@title <- input$uploadToDatabaseUI_title_Explorer
    tmp@abstract <- input$uploadToDatabaseUI_abstract_Explorer
    tmp@url <- input$uploadToDatabaseUI_url_Explorer
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_Explorer
    metadata(rs)$experimentData <- tmp
    saveRDS(rs, file = paste0("usrDatabase/",annotation(rs),"_TS.RDS"))
    output$fileSaveDatabase1 <- renderText(HTML("File uploaded to Database"))
    show("fileSaveDatabase1")
  })

  if(!is.null(rs)){
    parameterNamesME <- sracipeGenParamNames(rs)
    parametersME <- sracipeParams(rs)
  }


  ##################### Parameter Modification #######################
  output$modelParams <- renderUI({
    if(is.null(parametersME)) return(NULL)


    selectInput("selectedParameterME", "Parameter",
                parameterNamesME,
                selected = NULL)
  })

  output$newModelParamValue <- renderUI({
    if(is.null(parametersME)) return(NULL)
    textInput("parametervalue", "New value", 
              placeholder = parametersME[input$selectedParameterME], 
              value = parametersME[input$selectedParameterME])
  })

  newParamsME <- reactive({
    if (is.null(parametersME)) {
      return(NULL)
    }
    parametersME[input$selectedParameterME] <- input$parametervalue
    return(parametersME)
  })

  observeEvent(input$simulateModifiedME, {
    sracipeParams(rs) <- newParamsME()
    sracipeConfig(rs)$simParams["integrateStepSize"] <- 
      reactVariables$stepSizeExplorer()
    sracipeConfig(rs)$simParams["simulationTime"] <-   
      reactVariables$simTimeExplorer()
    sracipeConfig(rs)$stochParams["initialNoise"] <- input$noiseLevel
    withProgress(message = 'Simulating', value = 0.25, {

    rs <- sracipeSimulate(rs, timeSeries = TRUE, genIC = FALSE, 
                          genParams = FALSE, plots = FALSE)
    })
    output$modifiedMEts <- renderPlot({
      if(input$simulateModifiedME == 0) return()
      withProgress(message = 'Plotting', value = 0.25, {
        plotData <- t(metadata(rs)$timeSeries)
        .sracipePlotTS(plotData)
      })
    })
  })
  
  observeEvent(input$savedModifiedMEData,{
    saveRDS(rs, file = paste0("usrDatabase/",annotation(rs),"_TS.RDS"))
    output$fileSaveModifiedMEDatabase <- renderText(HTML("File uploaded to Database"))
    show("fileSaveModifiedMEDatabase")

  })

  ##################### Bifurcation Diagram #######################
  output$modelParamsBif <- renderUI({
    if(is.null(newParamsME())) return(NULL)

    selectInput("selectedParameterMEBif", "Parameter",
                parameterNamesME,
                selected = NULL)
  })

  output$modelParamBifMin <- renderUI({
    if(is.null(parametersME)) return(NULL)
    textInput("parameterValueBifMin", "Min value", 
              value = 0.9*parametersME[input$selectedParameterMEBif], 
              placeholder = 0.9*parametersME[input$selectedParameterMEBif])
  })

  output$modelParamBifMax <- renderUI({
    if(is.null(parametersME)) return(NULL)
    textInput("parameterValueBifMax", "Max value", 
              value = 1.1*parametersME[input$selectedParameterMEBif], 
              placeholder = 1.1*parametersME[input$selectedParameterMEBif])
  })

  newParamsMEBif <- reactive({
    if (is.null(newParamsME())) {
      return(NULL)
    }
    newParametersME <- newParamsME()[rep(seq_len(nrow(newParamsME())), 
                                         each=reactVariables$modelNumBifurs()),]
    modPar <- seq(from = input$parameterValueBifMin, 
                  to = input$parameterValueBifMax,
                  length.out = reactVariables$modelNumBifurs())
    newParametersME[input$selectedParameterMEBif] <- modPar
    return(newParametersME)
  })

  observeEvent(input$bifurcationME, {

    sracipeParams(rs) <- newParamsMEBif()
    sracipeConfig(rs)$simParams["numModels"] <- nrow(newParamsMEBif())
    withProgress(message = 'Simulating', value = 0.25, {

    rs <- sracipeSimulate(rs, genIC = TRUE, genParams = FALSE)
    })
    # Prevent grpah changes when parameter values are changed.
    # Change only when bifurcationME is clicked
    output$modifiedBifME <- renderPlot({
      if(input$bifurcationME == 0) return()
      withProgress(message = 'Plotting', value = 0.25, {
      library(ggplot2)
      exprs <- assays(rs)
      #exprs <- log2(exprs)
      sexprs <- stack(as.data.frame(exprs))
      colnames(sexprs) <- c("geneExp", "Gene")
      isolate(sexprs$bifurParameter <- rep(seq(
        from = input$parameterValueBifMin, to = input$parameterValueBifMax,
        length.out = reactVariables$modelNumBifurs()), ncol(exprs)))
      theme_set(theme_bw(base_size = 18))
      isolate(qplot(bifurParameter, geneExp, data = sexprs, group = Gene, 
                    colour = Gene, geom = "point", ylab = "Gene Expression", 
                    xlab = input$selectedParameterMEBif ))
})
      # plotRSet(rs, "exprsHeatmap")
    })
  }
  )
  # output$downloadBifData <- downloadHandler(
  #   filename <- paste0(annotation(rs),".RDS" ),
  #   content = function(con) {
  #     saveRDS(rs, con)
  #   }
  # )

  # observeEvent(input$saveBifData,{
  #   saveRDS(rs, file = paste0("usrDatabase/",annotation(rs),"_BIF.RDS"))
  #   output$fileSaveBifDatabase <- renderText(HTML("File uploaded to Database"))
  #   show("fileSaveBifDatabase")
  #
  # })

})




