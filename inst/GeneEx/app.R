# source('uploadModule.R')
# source('utility.R')


# Data and functions shared across all sessions in the same R process
# Utility functions not depending on output and input
# The database object
library(shinyjs)
source('allSessions.R')
source('geneExServer.R', local = TRUE)
source('geneExUI.R', local = TRUE)


shinyApp(
  ui = geneExUI,
  server = geneExServer
)


