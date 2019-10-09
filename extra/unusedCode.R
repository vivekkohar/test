# TO DO List
# Make the table editable?
# Check the error messages. Use safe error if needed.
# To remove files from a folder on exit
session$onSessionEnded(function() { unlink("tmp/*", recursive = TRUE) })


# This generates the circuit input UI from a module. This is useful if same UI
# is used at multiple places.
shinyModelExplorer <- callModule(shinyLoadNetwork, "shinyModelExplorerNetwork", stringsAsFactors = FALSE)
# Reload a session
# session$reload()


## Code for importing circuits from stored networks. May be used later
# tpoFiles <- tools::file_path_sans_ext(list.files(path = "inputs/", pattern = ".tpo", all.files = FALSE,
#                                                  full.names = FALSE, recursive = FALSE,
#                                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
# output$storedTpoFiles <- renderUI({
#
#   if(is.null(tpoFiles)) return(NULL)
#
#   selectInput("selectedTpoFile", "Stored Circuit",
#               tpoFiles,
#               selected = NULL)
# })