
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

suppressMessages(library(shiny))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(gplots))
suppressMessages(library(MASS))
suppressMessages(library(RColorBrewer))
suppressMessages(library(DT))
suppressMessages(library(visNetwork))
suppressMessages(library(shinyjs))
suppressMessages(library(sRACIPE))
suppressMessages(library(data.table))
suppressMessages(library(shinyBS))
# library(htmlwidgets)
# library(d3heatmap)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot_color <- rf(32)
options(shiny.sanitize.errors = FALSE)

geneExServer <- shinyServer(function(input, output, session) {
  source('circuitServer.R', local = TRUE)
  source('modelExplorerServer.R', local = TRUE)
  source('racipeServer.R', local = TRUE)
  source('validateServer.R', local = TRUE)
  source('databaseServer.R', local = TRUE)

  session$onSessionEnded(stopApp)
})

