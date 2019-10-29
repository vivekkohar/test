library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3,offset = 0,
           uiOutput('undoUI')),
    column(3,offset = 0,
           actionButton(
             "addInteraction","Add Interaction", 
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  fluidRow(
    DT::dataTableOutput("circuitTable"),
    # uiOutput("circuitTable"),
    shinyBS::bsTooltip(
      "circuitTable", "Circuit interactions.",
      placement = "bottom", trigger = "hover", options = NULL),
    fileInput(("circuitFile"),label = "Choose Circuit File"),
    shinyBS::bsTooltip("circuitFile", "Input the circuit using a file.",
                       placement = "bottom", trigger = "hover", options = NULL),
    textInput(("circuitName"), "Circuit Name", "Circuit1"),
    shinyBS::bsTooltip(
      "circuitName", "Name of the circuit which will be used as
    annotation for RacipeSE object", placement = "bottom", 
      trigger = "hover", options = NULL),
    downloadButton(('downloadCircuit'), 'Download Circuit'),
    shinyBS::bsTooltip(
      "downloadCircuit", "Download the circuit as a text file.", 
      placement = "bottom", trigger = "hover", options = NULL)
    
  ),
  
  fluidRow(
    column(3,offset = 5,
           actionButton(
             "updateCircuit", "Load Circuit",icon("thumbs-up"),
             style="color: #fff; background-color: #32CD32; 
           border-color: #2e6da4")
    )
  ),
  shinyBS::bsTooltip(
    "updateCircuit", "If the interactions look good, you can 
  Load the circuit for further analysis. 
  It will update the circuit figures.", placement = "bottom", 
    trigger = "hover", options = NULL),
  
  fluidRow(  visNetwork::visNetworkOutput("circuit")),
  
  hr()
)

server <- function(input, output) {
  circuitVariables <- reactiveValues(
    # rSet <- NULL,
   # deletedRows <- NULL,
 #   deletedRowIndices <- list()
  )
  circuitVariables$circuit <- data.frame(
    Source = c("A", "B"),
    Target = c("B", "A"),
    Interaction = c(2, 2)
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
  
  
  output$downloadCircuit <- downloadHandler(
    filename = function() {
      if(is.null(circuitVariables$rSet)) return("circuit.txt")
      paste(annotation(circuitVariables$rSet), '.txt', sep='')
    },
    content = function(con) {
      if(is.null(circuitVariables$rSet)) { data = data.frame(
        Source = c("A", "B"),
        Target = c("B", "A"),
        Interaction = c(2, 2))}
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
    dataRow <- data.frame(Source = "srcGene", Target = "tgtGene", Interaction = 1)
    
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
  
  
}

#' Adds a row at a specified index
#'
#' @param df a data frame
#' @param row a row with the same columns as \code{df}
#' @param i the index we want to add row at.
#' @return the data frame with \code{row} added to \code{df} at index \code{i}
addRowAt <- function(df, row, i) {
  # Slow but easy to understand
  if (i > 1) {
    rbind(df[1:(i - 1), ], row, df[-(1:(i - 1)), ])
  } else {
    rbind(row, df)
  }
  
}

#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ),editable = TRUE)
}

#' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

# Run the application
shinyApp(ui = ui, server = server)