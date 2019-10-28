# Initialize the network to NULL
# Do not use simple reactive as we update the network from different places
circuitVariables <- reactiveValues()
circuitVariables$circuit <- reactive(data.table(
#  if(input$updateTopologyfromText ==0) {
#   if(input$updateTopologyfromFile ==0) {
     Source = c("A", "B"),
                        Target = c("B", "A"),
                        Interaction = c(2, 2))
#  }
#  }
)
# output$circuitTable <-renderDT({
#   # return(isolate(circuitVariables$circuit()))
#   return(data.frame(Source = c("A", "B"),
#              Target = c("B", "A"), 
#              Interaction = c("2", "2")))
# }, selection = 'none', editable = TRUE, rownames = FALSE,filter = 'top'
# )

output$circuitTable<-renderUI({
  fluidPage(
#    box(width=12,
        h3(strong("Circuit Interactions"),align="center"),
        hr(),

        
        column(12,dataTableOutput("mainTable")),
        column(6,offset = 6,
HTML('<div class="btn-group" role="group" aria-label="Circuit Interaction Table">'),
               actionButton(inputId = "addInteraction",
                            label = "Add interaction",icon("plus-square"),
 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               HTML('</div>')
        ),
        tags$script(HTML('$(document).on("click", "input", function () {
               var checkboxes = document.getElementsByName("selectedRow");
                         var checkboxesChecked = [];
                         for (var i=0; i<checkboxes.length; i++) {
                         
                         if (checkboxes[i].checked) {
                         checkboxesChecked.push(checkboxes[i].value);
                         }
                         }
                         Shiny.onInputChange("checkedRows",checkboxesChecked);
})')),
      tags$script("$(document).on('click', '#mainTable button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
                  });")

#      )
      )
    })
  
output$mainTable<-renderDataTable({
  circuitTmp <- circuitVariables$circuit()
  DT=circuitTmp

  DT[["Actions"]]<-
    paste0('
<div class="btn-group" role="group" aria-label="Circuit Interaction Table">
<button type="button" class="btn btn-secondary modify"id=modify_',
1:nrow(circuitTmp),'>Modify</button>
<button type="button" class="btn btn-secondary delete" id=delete_',
1:nrow(circuitTmp),'>Delete</button>
           </div>
           
           ')
  datatable(DT,
            escape=FALSE, options = list(pageLength = 100))}
    )

observeEvent(input$addInteraction,{
  newInteraction=data.frame(
    Source = "SrcGene",
    Target = "TgtGene",
    Interaction = 1)
  
  circuitVariables$circuit <- reactive(rbind(circuitTmp,
                                             newInteraction))
#  showModal(modalModify2)
})


##Managing in row deletion
modal_modify<-modalDialog(
  fluidPage(
    h3(strong("Interaction"),align="center"),
    hr(),
    dataTableOutput('row_modif'),
    
    
    tags$script(HTML("$(document).on('click', '#save_changes', function () {
                     var list_value=[]
                     for (i = 0; i < $( '.new_input' ).length; i++)
                     {
                     list_value.push($( '.new_input' )[i].value)
                     }
                     
                     Shiny.onInputChange('newValue', list_value)
                     });"))
    ),
  size="l", footer = 
    actionButton("save_changes","Save changes"),easyClose = TRUE
    )

observeEvent(input$save_changes, {
  removeModal()
} )
observeEvent(input$lastClick,
             {
               circuitTmp <- circuitVariables$circuit()
               if (input$lastClickId%like%"delete")
               {
                 row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                 circuitVariables$circuit=circuitTmp[-row_to_del]
               }
               else if (input$lastClickId%like%"modify")
               {
                 showModal(modal_modify)
               }
             }
)

output$row_modif<-renderDataTable({
  circuitTmp <- circuitVariables$circuit()
  selected_row=as.numeric(gsub("modify_","",input$lastClickId))
  old_row=circuitTmp[selected_row]
  row_change=list()
  for (i in colnames(old_row))
  {
    if (is.numeric(circuitTmp[[i]]))
    {
row_change[[i]] <-
  paste0('<input class="new_input" type="number" id=new_',i,'><br>')
    }
    else
      row_change[[i]] <-
        paste0('<input class="new_input" type="text" id=new_',i,'><br>')
  }
  row_change=as.data.table(row_change)
  setnames(row_change,colnames(old_row))
  DT=rbind(old_row,row_change)
  rownames(DT)<-c("Current values","New values")
  DT
  
},escape=FALSE,options=list(dom='t',ordering=FALSE),selection="none"
)

modalModify2 <-modalDialog(
  fluidPage(
    h3(strong("Interaction"),align="center"),
    hr(),
    dataTableOutput('rowModif2'),
    
    tags$script(HTML("$(document).on('click', '#save_changes', function () {
                     var list_value=[]
                     for (i = 0; i < $( '.new_input' ).length; i++)
                     {
                     list_value.push($( '.new_input' )[i].value)
                     }
                     
                     Shiny.onInputChange('newValue', list_value)
                     });"))
    ),
  size="l", footer = 
    actionButton("save_changes","Save changes"),easyClose = TRUE
    )

# observeEvent(input$saveChanges2, {
#   removeModal()
# } )

output$rowModif2 <- renderDataTable({
  circuitTmp <- circuitVariables$circuit()
  selected_row=(nrow(circuitTmp))
  
  old_row=circuitTmp[selected_row]
  row_change=list()
  for (i in colnames(old_row))
  {
    if (is.numeric(circuitTmp[[i]]))
    {
      row_change[[i]]<-
        paste0('<input class="new_input" type="number" id=new_',i,'><br>')
    }
    else
      row_change[[i]]<-
        paste0('<input class="new_input" type="text" id=new_',i,'><br>')
  }
  row_change=as.data.table(row_change)
  setnames(row_change,colnames(old_row))
  DT=rbind(old_row,row_change)
  rownames(DT)<-c("Current values","New values")
  DT
  
},escape=FALSE,options=list(dom='t',ordering=FALSE),selection="none"
)

observeEvent(input$newValue,
             {
               newValue=lapply(input$newValue, function(col) {
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                   as.numeric(as.character(col))
                 } else {
                   col
                 }
               })
               DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
               colnames(DF)=colnames(circuitVariables$circuit())
circuitVariables$circuit[as.numeric(gsub("modify_","",input$lastClickId))] <- 
  reactive(DF)
               
             }
)






# # Update the network if [Load Circuit] button is clicked from [Enter Text] tab
# observeEvent(input$updateTopologyfromText,{
#   inputText <- isolate(input$uiTopology)
#   inputText <- gsub("\t",",", inputText)
#   inputText <- gsub("\n",",", inputText)
#   inputText <- gsub(" ",",", inputText)
#   
#   circuitVariables$explorerNetwork <- isolate(
#     reactive(read.table(text=inputText,
#                         col.names=c('Source','Target', 'Interaction'),
#                         sep = ",", stringsAsFactors = FALSE)))
#   
#   # flush the outputs if network is reset
#   output$MEts <- renderPlot({   return()  })
#   output$modifiedMEts <- renderPlot({ return()})
#   output$modifiedBifME <- renderPlot({ return()})
#   
# })
# 
# # Update the network if [Load Circuit] button is clicked from [Load File] tab
# observeEvent(input$updateTopologyfromFile,{
#   data <-  input$file
#   if(is.null(data)) {
#     return(NULL)
#   }
#   reactVariables$explorerNetwork <- reactive( isolate(read.table(
#     data$datapath,sep="", header =TRUE, stringsAsFactors = FALSE)))
#   # update the [Enter Text] with the loaded circuit
#   updateTextAreaInput(session, "uiTopology", 
#                       value = as.matrix(reactVariables$explorerNetwork()))
#   
#   # flush the outputs if network is reset
#   output$MEts <- renderPlot({   return()  })
#   output$modifiedMEts <- renderPlot({ return()})
#   output$modifiedBifME <- renderPlot({ return()})
#   
# })
# 
# # Message on how to enter the circuit as text
# output$networkTextFormat <- renderUI({HTML(
#   "Enter the interactions separating the source, 
# target and interactions type by
#   space or tab or comma.\n
#   Use 1 for activation and 2 for inhibition. For example,\n
#   srcA,tgtA,1,srcB,tgtB,2,srcA,tgtB,2")})
# 
# 



observeEvent(input$updateCircuit,{
# circuitVariables$rSet <- reactive({
#  if(input$updateTopologyfromText ==0) {
#    if(input$updateTopologyfromFile ==0) {
#      if(input$loadNetworkExplorer == 0)
#  if(is.null(circuitVariables$circuit())){
#    { return ()}
#    else
  {
    rSet <-  RacipeSE()
    sracipeCircuit(rSet) <- circuitVariables$circuit()
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
    }
  #  }
#  }
})
# observeEvent(input$useSampleCircuit,{
#   
#   circuitVariables$circuit <- data.frame(Source = c("A", "B"),
#                                          Target = c("B", "A"), 
#              Interaction = c("2", "2"))
#   
# })
# 
# 
output$circuit <- renderVisNetwork({
#  if(input$updateTopologyfromText ==0) {
#    if(input$updateTopologyfromFile ==0) {
#      if(input$loadNetworkExplorer == 0)
#      { return (NULL)}
#    }
#  }
  if(is.null(circuitVariables$rSet)) { return (NULL)}
  sracipePlotCircuit(circuitVariables$rSet, plotToFile = FALSE)
})

  observeEvent(input$circuitFile,{
    circuitData <- input$circuitFile

circuitVariables$circuit <- reactive(read.table(
       circuitData$datapath,sep=" ", header =TRUE, stringsAsFactors = FALSE))
  
  })
  
  
output$downloadCircuit <- downloadHandler(
    filename = function() {
      if(is.null(circuitVariables$rSet)) return("circuit.tpo")
      paste(annotation(circuitVariables$rSet), '.tpo', sep='')
    },
    content = function(con) {
      if(is.null(circuitVariables$rSet)) { data = data.table(
        Source = c("A", "B"),
        Target = c("B", "A"),
        Interaction = c(2, 2))}
      else {data = sracipeCircuit(circuitVariables$rSet)}
      write.table(data, con, 
                  row.names = FALSE, quote = FALSE)
    }
)
