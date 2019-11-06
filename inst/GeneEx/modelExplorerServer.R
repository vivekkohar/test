###########################################
# Model Explorer
###########################################
gvVals = reactiveValues()
demoCircuit = data.table(
  Source = c("A", "B"),
  Target = c("B", "A"),
  Interaction = c(2, 2))
rSetTmp <- sracipeSimulate(circuit = demoCircuit, integrate = FALSE, 
                           numModels = 1)
annotation(rSetTmp) <- "Circuit1"
gvVals$rSet <- reactive(rSetTmp)
gvVals$parameterNamesME <- reactive(sracipeGenParamNames(rSetTmp))
gvVals$parametersME <- reactive(sracipeParams(rSetTmp))
gvVals$rSetBifur <- reactive(rSetTmp)
######################### Network Input ######################################

output$circuitGv <- renderVisNetwork({
  if(is.null(gvVals$rSet())) { return (NULL)}
  rSet <- gvVals$rSet()
  sracipePlotCircuit(rSet, plotToFile = FALSE)
})
# Array to hold reactive variables like network topology



# Limit maximum time to 5000.
gvVals$simTimeExplorer <- reactive({
#  if(input$simTimeExplorer > 5000) return(5000)
  # else
    return(input$simTimeExplorer)
})
# Use maximum and minimum limits on the initial step size
gvVals$stepSizeExplorer <- reactive({
  if(input$stepSizeExplorer > 1) return(0.99)
  if(input$stepSizeExplorer < .0001) return(0.0001)
  else return(input$stepSizeExplorer)
})

gvVals$initialNoise <- reactive({
  #if(input$noiseLevel > 5000) return(5000)
  # else 
    return(input$noiseLevel)
})

gvVals$nNoise <- reactive({
  #if(input$noiseLevel > 5000) return(5000)
  # else 
  n = 0L
  if (input$noiseLevel>0) n = 1L
  n
})
##################### Parameter Modification #######################

output$modelParams <- renderUI({
     if(is.null(gvVals$parametersME())) return(NULL)
  selectInput("selectedParameterME", "Parameter",
              gvVals$parameterNamesME(),
              selected = NULL)
})

output$newModelParamValue <- renderUI({
  if(is.null(gvVals$parametersME())) return(NULL)
  textInput("parametervalue", "Value",
            placeholder = gvVals$parametersME()[input$selectedParameterME],
            value = gvVals$parametersME()[input$selectedParameterME])
  
})

observeEvent(input$updateGvParam,{
  params <- gvVals$parametersME()
  params[input$selectedParameterME] <- as.numeric(input$parametervalue)
  gvVals$parametersME <- reactive(params)
})
observeEvent(input$simulateGv, {
  withBusyIndicatorServer("simulateGv",{
  # if(!is.numeric(input$simTimeExplorer) | !is.numeric(input$stepSizeExplorer)
  # | !is.numeric(input$noiseLevel)) {
  #   createAlert(session, "gvAlert", "numericAlert", title = "Oops",
  #               content = "I only understand numeric values!", append = TRUE)
  # } else if((input$simTimeExplorer < 0) | (input$stepSizeExplorer <0 ) | 
  # (input$noiseLevel) <0 ) {
  #   createAlert(session, "gvAlert", "numericAlert", title = "Oops",
  #               content = "I can work with only positive values!", 
  # append = TRUE)
  # } else if((input$simTimeExplorer >500) | (input$stepSizeExplorer > 1 )
  # | (input$noiseLevel) > 50 ) {
  #   createAlert(session, "gvAlert", "numericAlert", title = "Oops",
  #               content = "I think the value is too high!", append = TRUE)
  # } else {
  #    closeAlert(session, "numericAlert")

# {
  shinyjs::show("bifurcationExplorer")
  shinyjs::show("downloadMEData")
  shinyjs::show("downloadMEDataType")
  shinyjs::show("saveMEData")
  shinyjs::show("GvTS")
  rs <- gvVals$rSet()
#  withProgress(message = 'Please wait...', value = 0.0, {
  sracipeParams(rs) <- gvVals$parametersME()
   
 #  sracipeConfig(rs)$simParams["integrateStepSize"] <- 
  # gvVals$stepSizeExplorer() #input$stepSizeRacipe
 #  sracipeConfig(rs)$simParams["simulationTime"] <-   input$simTimeRacipe
   
   rs <- sracipeSimulate(rs, timeSeries = TRUE, plots = FALSE, genIC = FALSE, 
                         genParams = FALSE, integrate = TRUE,
                        simulationTime  = isolate(gvVals$simTimeExplorer()),
                       integrateStepSize = isolate(gvVals$stepSizeExplorer()),
                       initialNoise = isolate(gvVals$initialNoise()), 
                       nNoise = isolate(gvVals$nNoise()) 
                       )
  gvVals$rSet <- reactive(rs)
  # if(input$emailGv){
  #   zipFile <- paste0(tempfile(),annotation(gvVals$rSet()),".zip")
  #   files <- NULL;
  #   fileName <- paste(tempdir(),"/",annotation(gvVals$rSet()),"_GE",".txt",sep = "")
  #   write.table(metadata(gvVals$rSet())$timeSeries,fileName,sep = ' ', 
  #               row.names = TRUE, 
  #               col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  # 
  #   fileName <- paste(tempdir(),"/",annotation(gvVals$rSet()),"_network",".txt",sep = "")
  #   write.table(sracipeCircuit(gvVals$rSet()),fileName,sep = ' ', 
  #               row.names = FALSE, 
  #               col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  #   
  #   fileName <- paste(tempdir(),"/",annotation(gvVals$rSet()),"_params",".txt",sep = "")
  #   write.table(sracipeParams(gvVals$rSet()),fileName,sep = ' ', 
  #               row.names = TRUE, 
  #               col.names = TRUE, quote = FALSE)
  #   files <- c(fileName,files)
  #   
  #   #create the zip file
  #   zip(zipFile,files)
  #   googledrive::drive_upload(zipFile,path = "download",
  #                             name = annotation(gvVals$rSet()), overwrite = TRUE)
  #   googledrive::drive_share(annotation(gvVals$rSet()),role = "reader",type = "anyone")
  #   fileLink <- googledrive::drive_link(annotation(gvVals$rSet()))
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
#})
  output$GvTS <- renderPlot({

    if(input$simulateGv == 0) return()
  #  withProgress(message = 'Plotting', value = 0.25, {
      plotData <- t(metadata(gvVals$rSet())$timeSeries)
      .sracipePlotTS(plotData)
  #  })
})
  })
  # output$info <- renderText({
  #   paste0("x=", input$plot_click_MEts$x, "\ny=", input$plot_click_MEts$y)
  # })

  output$downloadMEData <- downloadHandler(
      filename = function(){
        if(input$downloadMEDataType == "txt"){
        return(paste0(annotation(gvVals$rSet()),".zip"))}
        else return(paste0(annotation(gvVals$rSet()),".RDS"))
      },

      content = function(file){
        if(input$downloadMEDataType == "txt"){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        fileName <- paste(annotation(gvVals$rSet()),"_GE",".txt",sep = "")
        write.table(metadata(gvVals$rSet())$timeSeries,fileName,sep = ' ', 
                    row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        # fileName <- paste(annotation(rs),"_ic",".csv",sep = "")
        # write.table(ic(rs),fileName,sep = ',', row.names = F, col.names = T)
        # files <- c(fileName,files)

        fileName <- paste(annotation(gvVals$rSet()),"_network",".txt",sep = "")
        write.table(sracipeCircuit(gvVals$rSet()),fileName,sep = ' ', 
                    row.names = FALSE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        fileName <- paste(annotation(gvVals$rSet()),"_params",".txt",sep = "")
        write.table(sracipeParams(gvVals$rSet()),fileName,sep = ' ', 
                    row.names = TRUE, 
                    col.names = TRUE, quote = FALSE)
        files <- c(fileName,files)

        #create the zip file
        zip(file,files)

        
        
        }
        else{
            saveRDS(gvVals$rSet(), file)
      }
    })

  observeEvent(input$saveMEData,{
    shinyjs::show("uploadMEData")
    shinyjs::show("uploadToDatabaseUI_name_Explorer")
    shinyjs::show("uploadToDatabaseUI_lab_Explorer")
    shinyjs::show("uploadToDatabaseUI_contact_Explorer")
    shinyjs::show("uploadToDatabaseUI_title_Explorer")
    shinyjs::show("uploadToDatabaseUI_abstract_Explorer")
    shinyjs::show("uploadToDatabaseUI_url_Explorer")
    shinyjs::show("uploadToDatabaseUI_pubMedIds_Explorer")
    shinyjs::hide("fileSaveDatabase1")
  })

  observeEvent(input$uploadMEData,{
    withBusyIndicatorServer("uploadMEData",{
    rSet <- gvVals$rSet()
    tmp <- MIAME()
    tmp@name <- input$uploadToDatabaseUI_name_Explorer
    tmp@lab <- input$uploadToDatabaseUI_lab_Explorer
    tmp@contact <- input$uploadToDatabaseUI_contact_Explorer
    tmp@title <- input$uploadToDatabaseUI_title_Explorer
    tmp@abstract <- input$uploadToDatabaseUI_abstract_Explorer
    tmp@url <- input$uploadToDatabaseUI_url_Explorer
    tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds_Explorer
    metadata(rSet)$experimentData <- tmp
    #saveRDS(rSet, file = paste0("usrDatabase/",Sys.time(),"_",annotation(rSet),"_TS.RDS"))
    filename <- paste(tempdir(),"/",annotation(rSet),".RDS", sep = "")
    saveRDS(rSet, file = filename)
    googledrive::drive_upload(filename,path = "upload",name = annotation(rSet), overwrite = TRUE)
    })
    output$fileSaveDatabase1 <- renderText(HTML("File uploaded to Database"))
    shinyjs::show("fileSaveDatabase1")
    shinyjs::hide("uploadMEData")
    shinyjs::hide("uploadToDatabaseUI_name_Explorer")
    shinyjs::hide("uploadToDatabaseUI_lab_Explorer")
    shinyjs::hide("uploadToDatabaseUI_contact_Explorer")
    shinyjs::hide("uploadToDatabaseUI_title_Explorer")
    shinyjs::hide("uploadToDatabaseUI_abstract_Explorer")
    shinyjs::hide("uploadToDatabaseUI_url_Explorer")
    shinyjs::hide("uploadToDatabaseUI_pubMedIds_Explorer")
    
  })

  ##################### Bifurcation Diagram #######################
  observeEvent(input$bifurcationExplorer,{
    shinyjs::show("modelParamsBif")
    shinyjs::show("modelParamBifMin")
    shinyjs::show("modelParamBifMax")
    shinyjs::show("modelNumBifurs")
    shinyjs::show("bifurcationME")
    shinyjs::show("modifiedBifME")
    
  })
  # Limit the maximum number of models to 5000.

  output$modelParamsBif <- renderUI({
    if(is.null(gvVals$rSet())) return(NULL)
    selectInput("selectedParameterMEBif", "Parameter",
                gvVals$parameterNamesME(),
                selected = NULL)
  })

  output$modelParamBifMin <- renderUI({
    if(is.null(gvVals$parametersME())) return(NULL)
    textInput("parameterValueBifMin", "Min value", 
              value = 0.9*gvVals$parametersME()[input$selectedParameterMEBif], 
       placeholder = 0.9*gvVals$parametersME()[input$selectedParameterMEBif])
  })

  output$modelParamBifMax <- renderUI({
    if(is.null(gvVals$parametersME())) return(NULL)
    textInput("parameterValueBifMax", "Max value", 
              value = 1.1*gvVals$parametersME()[input$selectedParameterMEBif], 
       placeholder = 1.1*gvVals$parametersME()[input$selectedParameterMEBif])
  })


  observeEvent(input$bifurcationME, {
    withBusyIndicatorServer("bifurcationME",{
    shinyjs::show("downloadBifData")
    modelNumBifurs <-   input$modelNumBifurs
    if(modelNumBifurs > 5000) modelNumBifurs <- 5000

    newParametersME <- 
      gvVals$parametersME()[rep(seq_len(nrow(gvVals$parametersME())), 
                                                      modelNumBifurs),]
    modPar <- seq(from = as.numeric(input$parameterValueBifMin), 
                  to = as.numeric(input$parameterValueBifMax),
                  length.out = modelNumBifurs)
    newParametersME[input$selectedParameterMEBif] <- modPar
    rs <- RacipeSE()
    
    sracipeCircuit(rs) <- sracipeCircuit(gvVals$rSet())
    rs <- sracipeSimulate(rs, genIC = TRUE, genParams = TRUE, integrate = FALSE,
                          initialNoise = 0, nNoise = 0, 
                          numModels = modelNumBifurs)
    sracipeParams(rs) <- newParametersME
    # sracipeConfig(rs)$simParams["numModels"] <- modelNumBifurs
 #   withProgress(message = 'Simulating', value = 0.25, {

    rs <- sracipeSimulate(rs, genIC = TRUE, genParams = FALSE, integrate = TRUE,
                          initialNoise = 0, nNoise = 0, 
                          numModels = modelNumBifurs)
    gvVals$rSetBifur <- reactive(rs)
    # print(assays(gvVals$rSetBifur)[[1]])
    gvVals$modelNumBifurs <- reactive(modelNumBifurs)
    gvVals$newParametersME <- reactive(newParametersME)
 #   })
    # Prevent grpah changes when parameter values are changed.
    # Change only when bifurcationME is clicked
    output$modifiedBifME <- renderPlot({
      if(input$bifurcationME == 0) return()
    #  withProgress(message = 'Plotting', value = 0.25, {
      library(ggplot2)
      sexprs <- assays(gvVals$rSetBifur())[[1]]
      sexprs <- reshape2::melt(t(sexprs))
      colnames(sexprs) <- c("bifurParameter","Gene","geneExp")
     test <- isolate(rep(seq(
        from = as.numeric(input$parameterValueBifMin),
        to = as.numeric(input$parameterValueBifMax),
        length.out = gvVals$modelNumBifurs()), (ncol(sexprs)-1)))
      # print(test)
      
      sexprs$bifurParameter <- test
      #print(sexprs)
      theme_set(theme_bw(base_size = 18))
      isolate(qplot(isolate(bifurParameter), isolate(geneExp), 
                    data = isolate(sexprs), group = Gene, 
                    colour = Gene, geom = "point", ylab = "Gene Expression", 
                    xlab = isolate(input$selectedParameterMEBif )))
#})
      # plotRSet(rs, "exprsHeatmap")
    })
    })
  }
  )
  output$downloadBifData <- downloadHandler(
    filename <- paste0(Sys.time(),"_",annotation(gvVals$rSetBifur()),".RDS" ),
    content = function(con) {
      saveRDS(gvVals$rSetBifur(), con)
    }
  )

  # observeEvent(input$saveBifData,{
  #   saveRDS(rs, file = paste0("usrDatabase/",annotation(rs),"_BIF.RDS"))
  #  output$fileSaveBifDatabase <- renderText(HTML("File uploaded to Database"))
  #   show("fileSaveBifDatabase")
  #
  # })


  
  # }
})




