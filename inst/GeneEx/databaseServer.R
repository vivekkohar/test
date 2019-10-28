responseMsg1 <- ""
responseMsg1 <-  eventReactive(input$shinySelectNetworkDb,
                               "No network with matching name found!")
responseMsg2 <- ""
responseMsg2 <-  eventReactive(input$shinySelectGeneDb, 
                               "No network with this gene found!")

output$shinySelectedNetworkGene <- renderText(responseMsg1())
output$shinySelectedNetwork <- renderText(responseMsg2())
databaseVals <- reactiveValues()
databaseTable <- readRDS("data/GeneExDatabase_10112019.RDS")

output$databaseTable <- DT::renderDT({
  
  selectedRow <- reactive(input$databaseTable_row_last_clicked)
  # print(selectedRow())
  if(is.null(selectedRow())){return(databaseTable[,1:10])}
  observeEvent(input$databaseTable_row_last_clicked,{
    selectedRow <- reactive(input$databaseTable_row_last_clicked)
    # print(selectedRow())

    shinyjs::show("loadNetworkDatabase")
    shinyjs::hide("downloadDbData")
    shinyjs::hide("downloadDbDataType")
#    shinyjs::hide("plotDbNetworkExprs")
#    shinyjs::hide("plotDbPC")
    
    output$msg <- renderText("")
    # print("clicked")
    networkName <- databaseTable[selectedRow(),"Name"]
    # print(networkName)
    circuit <- data.frame(databaseTable[selectedRow(),"Circuit"]$Circuit  )
    rs <- RacipeSE()
    # print(circuit)
    sracipeCircuit(rs) <- circuit
    
    annotation(rs) <- networkName
    databaseVals$rSet <- reactive(rs)
    output$tableDbNetwork <- DT::renderDT({
      circuit
    }, options = list(
      pageLength = 100))
    
    output$plotDbNetwork <- renderVisNetwork({
      sracipePlotCircuit(rs, plotToFile = FALSE)
    })
    observeEvent(input$loadNetworkDatabase, {

      rSet <-  RacipeSE()
      circuitTmp <- sracipeCircuit(rs)
      circuitVariables$circuit <- data.table(circuitTmp)
      sracipeCircuit(rSet) <- circuitTmp
      annotation(rSet) <- annotation(rs)
      circuitVariables$rSet <- rSet
      racipeVals$rSet <- rSet
      gvVals$rSet <- reactive(sracipeSimulate(
        rSet,integrate = FALSE, numModels = 1))
      gvVals$parameterNamesME <- reactive(sracipeGenParamNames(gvVals$rSet()))
      gvVals$parametersME <- reactive(sracipeParams(gvVals$rSet()))
      
      shinyjs::hide("bifurcationExplorer")
      shinyjs::hide("downloadMEData")
      shinyjs::hide("downloadMEDataType")
      shinyjs::hide("saveMEData")
      shinyjs::hide("GvTS")
      
      shinyjs::hide("uploadMEData")
      shinyjs::hide("uploadToDatabaseUI_name_Explorer")
      shinyjs::hide("uploadToDatabaseUI_lab_Explorer")
      shinyjs::hide("uploadToDatabaseUI_contact_Explorer")
      shinyjs::hide("uploadToDatabaseUI_title_Explorer")
      shinyjs::hide("uploadToDatabaseUI_abstract_Explorer")
      shinyjs::hide("uploadToDatabaseUI_url_Explorer")
      shinyjs::hide("uploadToDatabaseUI_pubMedIds_Explorer")
      
      shinyjs::hide("fileSaveDatabase1")
      shinyjs::hide("uploadMEData")
      shinyjs::hide("uploadToDatabaseUI_name_Explorer")
      shinyjs::hide("uploadToDatabaseUI_lab_Explorer")
      shinyjs::hide("uploadToDatabaseUI_contact_Explorer")
      shinyjs::hide("uploadToDatabaseUI_title_Explorer")
      shinyjs::hide("uploadToDatabaseUI_abstract_Explorer")
      shinyjs::hide("uploadToDatabaseUI_url_Explorer")
      shinyjs::hide("uploadToDatabaseUI_pubMedIds_Explorer")
      shinyjs::hide("modelParamsBif")
      shinyjs::hide("modelParamBifMin")
      shinyjs::hide("modelParamBifMax")
      shinyjs::hide("modelNumBifurs")
      shinyjs::hide("bifurcationME")
      shinyjs::hide("modifiedBifME")
      shinyjs::hide("downloadBifData")
      
      shinyjs::hide("racipeDeterministicText")
      shinyjs::hide("racipeHeatmap")
      shinyjs::hide("racipePca")
      shinyjs::hide("parametricAnalysisRacipe")
      shinyjs::hide("paDescrpt")
      shinyjs::hide("filteredOutputRacipe")
      shinyjs::hide("filterSliderRacipe")
      shinyjs::hide("filteredOutputRacipe2")
      shinyjs::hide("filterSliderRacipe2")
      shinyjs::hide("filteredOutputRacipe3")
      shinyjs::hide("filterSliderRacipe3")
      shinyjs::hide("downloadDataRacipe")
      shinyjs::hide("downloadDataRacipeType")
      shinyjs::hide("saveDataRacipe")
      shinyjs::hide("racipeHeatmapFiltered")
      shinyjs::hide("racipePcaFiltered")
      shinyjs::hide("validateRacipe")
      
      shinyjs::hide("uploadDataRacipe")
      shinyjs::hide("uploadToDatabaseUI_name_Racipe")
      shinyjs::hide("uploadToDatabaseUI_lab_Racipe")
      shinyjs::hide("uploadToDatabaseUI_contact_Racipe")
      shinyjs::hide("uploadToDatabaseUI_title_Racipe")
      shinyjs::hide("uploadToDatabaseUI_abstract_Racipe")
      shinyjs::hide("uploadToDatabaseUI_url_Racipe")
      shinyjs::hide("uploadToDatabaseUI_pubMedIds_Racipe")
      
      shinyjs::hide("sRacipeOption")
      shinyjs::hide("sRacipeNoise")
      shinyjs::hide("simulateSRacipe")
      shinyjs::hide("sRacipeHeatmap")
      shinyjs::hide("sRacipeHeatmapDet")
      shinyjs::hide("sRacipePca")
      shinyjs::hide("sRacipePcaDet")
      
      shinyjs::hide("saveDataSRacipe")
      shinyjs::hide("downloadDataSRacipe")
      shinyjs::hide("downloadDataSRacipeType")
      
      shinyjs::hide("uploadDataSRacipe")
      shinyjs::hide("uploadToDatabaseUI_name_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_lab_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_contact_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_title_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_abstract_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_url_sRacipe")
      shinyjs::hide("uploadToDatabaseUI_pubMedIds_sRacipe")
    })
#    shinyjs::show("showExpressionDatabase")

    
 })
  
#   
#   observeEvent(input$showExpressionDatabase, {
#     shinyjs::show("downloadDbData")
#     shinyjs::show("downloadDbDataType")
#     shinyjs::show("plotDbNetworkExprs")
#     shinyjs::show("plotDbPC")
#     
#     withProgress(message = 'Loading', value = 0.25, {
#       shinyBS::createAlert(session, anchorId = "dbAlert",
#                            alertId =  "Processing", title = "Processing",
#                            content = "Please wait...", append = FALSE)
#       rs <- isolate(databaseVals$rSet())
#       rs <- sracipeSimulate(rs, numModels = 500, plots = FALSE, 
#                             integrateStepSize = 0.05, simulationTime = 50)
#       rs <- sracipeNormalize(rs)
#       plotData <- assay(rs,1)
#       output$plotDbNetworkExprs <- renderPlot({
#         
#         .sracipePlotHeatmap(plotData = plotData)
#       })
#       output$plotDbPC <- renderPlot({
#         
#         .sracipePlotPca(plotData = plotData)
#       })
#       shinyBS::closeAlert(session, alertId = "Processing")
#     })
#     
#     output$downloadDbData <- downloadHandler(
#       filename = function(){
#         if(input$downloadDbDataType == "txt"){
#           return(paste0(annotation(rs),".zip"))}
#         else return(paste0(annotation(rs),".RDS"))
#       },
#       content = function(file){
#         if(input$downloadDbDataType == "txt"){
#           owd <- setwd(tempdir())
#           on.exit(setwd(owd))
#           files <- NULL;
#           
#           fileName <- paste(annotation(rs),"_GE",".txt",sep = "")
#           write.table(assay(rs,1),fileName,sep = ' ', row.names = TRUE, 
#                       col.names = TRUE, quote = FALSE)
#           files <- c(fileName,files)
#           
#           fileName <- paste(annotation(rs),"_IC",".txt",sep = "")
#           write.table(sracipeIC(rs),fileName,sep = ' ', row.names = FALSE, 
#                       col.names = TRUE)
#           files <- c(fileName,files)
#           
#           fileName <- paste(annotation(rs),"_network",".txt",sep = "")
#           write.table(sracipeCircuit(rs),fileName,sep = ' ', row.names = FALSE, 
#                       col.names = TRUE, quote = FALSE)
#           files <- c(fileName,files)
#           
#           fileName <- paste(annotation(rs),"_params",".txt",sep = "")
#           write.table(sracipeParams(rs),fileName,sep = ' ', row.names = TRUE, 
#                       col.names = TRUE, quote = FALSE)
#           files <- c(fileName,files)
#           
#           #create the zip file
#           zip(file,files)
#         }
#         else{
#           saveRDS(rs, file)
#         }
#       }
#     )
#     
#   })
#   
  return(databaseTable[,1:10])  
}, selection = 'single', editable = FALSE, rownames= FALSE, 
options = list(pageLength = 5)
)

