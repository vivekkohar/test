
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


#options(gargle_oauth_cache = ".secrets");
#gargle::gargle_oauth_cache();
# gm_auth(email = "geneex.maintainer@gmail.com", path = "credentials/credentials.json")
# gmailr::gm_auth()

geneExServer <- shinyServer(function(input, output, session) {
  source('circuitServer.R', local = TRUE)
  source('modelExplorerServer.R', local = TRUE)
  source('racipeServer.R', local = TRUE)
  source('validateServer.R', local = TRUE)
  source('databaseServer.R', local = TRUE)

#  session$onSessionEnded(stopApp)
})

