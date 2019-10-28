database <-
  tabPanel("Database",
           useShinyjs(),
           tags$head(includeHTML("google.html")),


        br(),

           fluidRow(
             DTOutput("databaseTable")
           ),
        hr(),
bsAlert("dbAlert"),
hr(),

br(),
fluidRow(
  column(4,offset = 0,
        DTOutput("tableDbNetwork")
#         hidden(actionButton("showExpressionDatabase", "Show Expression Plots", 
# icon = NULL, width = NULL, 
# style="color: #fff; background-color: #32CD32; border-color: #2e6da4"))
        ),
  column(8,offset = 0,
         hidden(actionButton("loadNetworkDatabase", "Load Circuit", 
style="color: #fff;background-color: #32CD32; border-color: #2e6da4")),
         visNetworkOutput("plotDbNetwork")
  )

  ),
shinyjs::hidden(downloadButton('downloadDbData', 'Download Data')),
shinyjs::hidden(radioButtons("downloadDbDataType", "Format",
                             c("RDS" = "RDS","Text" = "txt") , 
                             selected = "RDS",
                             inline = TRUE)),

#    plotOutput("plotDbNetworkExprs"),
#    plotOutput("plotDbPC"),


hr(),
hr()
#print("~~~Disclaimer~~~~")
  )
