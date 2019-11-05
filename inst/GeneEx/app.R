# source('uploadModule.R')
# source('utility.R')


# Data and functions shared across all sessions in the same R process
# Utility functions not depending on output and input
# The database object
library(shinyjs)
source('allSessions.R')
source('geneExServer.R', local = TRUE)
source('geneExUI.R', local = TRUE)
#options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)
#gmailr::gm_deauth()
# gmailr::gm_auth(id = "517184753467-66jbtn033775257lirarcaemt1vk198j.apps.googleusercontent.com", scope = "full")
#gmailr::gm_auth(email = "geneex.maintainer@gmail.com", scope = "full", use_oob = TRUE)
shinyApp(
  ui = geneExUI,
  server = geneExServer
)


