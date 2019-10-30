# Initialize the network to NULL
# Use global vars instead of simple reactive as we update the network 
# from different places
circuitVariables <- reactiveValues()
circuitVariables$circuit <- data.frame(
  Source = c("A", "B"),
  Target = c("B", "A"),
  Interaction = c(2, 2), stringsAsFactors = FALSE
)
circuitVariables$deletedRows <- NULL 
circuitVariables$deletedRowIndices <- list()
observeEvent(input$circuitFile,{
  circuitData <- input$circuitFile
  
  circuitVariables$circuit <- read.table(
    circuitData$datapath,sep=" ", header =TRUE, stringsAsFactors = FALSE)
  circuitVariables$deletedRows <- NULL
  circuitVariables$deletedRowIndices = list()
  
})

output$downloadSampleCircuit <- downloadHandler(
  filename = "circuit.txt",
  content = function(con) {
 data = data.frame(
      Source = c("A", "B"),
      Target = c("B", "A"),
      Interaction = c(2, 2))
    write.table(data, con, 
                row.names = FALSE, quote = FALSE)
  }
)
output$downloadCircuit <- downloadHandler(
  filename = function() {
    if(is.null(circuitVariables$rSet)) return("circuit.txt")
    paste(annotation(circuitVariables$rSet), '.txt', sep='')
  },
  content = function(con) {
    if(is.null(circuitVariables$rSet)) { data = data.frame(
      Source = c("A", "B"),
      Target = c("B", "A"),
      Interaction = c(2, 2), stringsAsFactors = FALSE)}
    else {data = sracipeCircuit(circuitVariables$rSet)}
    write.table(data, con, 
                row.names = FALSE, quote = FALSE)
  }
)

# output$circuitTable <-renderDT({
#   # return(isolate(circuitVariables$circuit))
#   return(data.frame(Source = c("A", "B"),
#              Target = c("B", "A"),
#              Interaction = c("2", "2")))
# }, selection = 'none', editable = TRUE, rownames = FALSE,filter = 'top'
# )

observeEvent(input$circuitTable_cell_edit, {
  circuitVariables$circuit[input$circuitTable_cell_edit$row,
                        (input$circuitTable_cell_edit$col)] <- 
    input$circuitTable_cell_edit$value
})

observeEvent(input$addInteraction, {
  rowNum <- (1 + nrow(circuitVariables$circuit))
  dataRow <- data.frame(Source = "srcGene", Target = "tgtGene", Interaction = 1,
                        stringsAsFactors = FALSE)
  
  circuitVariables$circuit <- rbind(dataRow,circuitVariables$circuit)
})

observeEvent(input$deletePressed, {
  rowNum <- parseDeleteEvent(input$deletePressed)
  dataRow <- circuitVariables$circuit[rowNum,]
  
  # Put the deleted row into a data frame so we can undo
  # Last item deleted is in position 1
  circuitVariables$deletedRows <- rbind(dataRow, circuitVariables$deletedRows)
  circuitVariables$deletedRowIndices <- append(circuitVariables$deletedRowIndices, rowNum, after = 0)
  
  # Delete the row from the data frame
  circuitVariables$circuit <- circuitVariables$circuit[-rowNum,]
})

observeEvent(input$undoDelete, {
  if(nrow(circuitVariables$deletedRows) > 0) {
    row <- circuitVariables$deletedRows[1, ]
    circuitVariables$circuit <- addRowAt(
      circuitVariables$circuit, row, circuitVariables$deletedRowIndices[[1]])
    
    # Remove row
    circuitVariables$deletedRows <- circuitVariables$deletedRows[-1,]
    # Remove index
    circuitVariables$deletedRowIndices <- circuitVariables$deletedRowIndices[-1]
  }
})

# Disable the undo button if we have not deleted anything
output$undoUI <- renderUI({
  if(
    !is.null(circuitVariables$deletedRows) && 
    nrow(circuitVariables$deletedRows) > 0) {
    actionButton(
      'undoDelete', label = 'Undo Delete', icon('undo'), 
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  } else {
    actionButton('undo', label = 'Undo delete', icon('undo'), disabled = TRUE)
  }
})

output$circuitTable <- DT::renderDataTable(
  # Add the delete button column
  deleteButtonColumn(circuitVariables$circuit, 'delete_button')
)

observeEvent(input$updateCircuit,{
    rSet <-  RacipeSE()
    
    sracipeCircuit(rSet) <- circuitVariables$circuit
    annotation(rSet) <- isolate(input$circuitName)
    circuitVariables$rSet <- rSet
    racipeVals$rSet <- rSet
    gvVals$rSet <- reactive(sracipeSimulate(rSet,integrate = FALSE,
                                            numModels = 1))
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
    shinyjs::hide("h5")
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
    shinyjs::hide("sRacipePca")
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

output$circuit <- renderVisNetwork({
  if(is.null(circuitVariables$rSet)) { return (NULL)}
  sracipePlotCircuit(circuitVariables$rSet, plotToFile = FALSE)
})

