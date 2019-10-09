source('uploadModule.R')
source('utility.R')
source('geneExServer.R')
source('geneExUI.R', local = TRUE)
library(shinyjs)




shinyApp(
  ui = geneExUI,
  server = geneExServer
)
