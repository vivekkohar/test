# Module UI function
uploadToDatabaseUI <- function(id, label = "uploadToDatabaseUI") {
  # Create a namespace function using the provided id
  ns2 <- NS(id)

  tagList(
    fluidRow(
      column(3, offset = 1,
             textInput(ns2("uploadToDatabaseUI_name"), "Name", "")),
      column(3, offset = 0,
             textInput(ns2("uploadToDatabaseUI_lab"), "Lab", "")),
      column(3, offset = 0,
             textInput(ns2("uploadToDatabaseUI_contact"), "Contact", ""))
    ),
    fluidRow( column(10, offset = 1,
             textInput(ns2("uploadToDatabaseUI_title"), "Title", ""))
             ),
    fluidRow( column(10, offset = 1,
      textInput(ns2("uploadToDatabaseUI_abstract"), "Abstract", ""))
    ),
    fluidRow(
      column(3, offset = 1,
      textInput(ns2("uploadToDatabaseUI_url"), "URL", "")),
      column(3, offset = 0,
             textInput(ns2("uploadToDatabaseUI_pubMedIds"), "PubMed ID", ""))
    )
  )
}
# Module server function
uploadToDatabaseServer <- function(input, output, session, stringsAsFactors) {
  tmp <- new("MIAME")
  tmp@name <- input$uploadToDatabaseUI_name
  tmp@lab <- input$uploadToDatabaseUI_lab
  tmp@contact <- input$uploadToDatabaseUI_contact
  tmp@title <- input$uploadToDatabaseUI_title
  tmp@abstract <- input$uploadToDatabaseUI_abstract
  tmp@url <- input$uploadToDatabaseUI_url
  tmp@pubMedIds <- input$uploadToDatabaseUI_pubMedIds
  return(tmp)
  }
