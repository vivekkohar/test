database <-
  tabPanel("Database",
           useShinyjs(),
           tags$head(includeHTML("google.html")),
           fluidRow(
             column(3, offset = 0,
                textInput("shinySelectNetworkDb",
                            "Enter Network Name", placeholder =  "EMT_npjSystemsBiology")),
             column(1, offset = 0,
                    br(),
             actionButton("submitNetwork", "Explore Network", icon = NULL, width = NULL)
             )
          #      column(1, offset = 0,    img(src='JAX.gif', align = "right"))
          ),

textOutput("msg"),

        br(),

           fluidRow(
             DTOutput("databaseTable")
           ),
        hr(),


br(),
fluidRow(
  column(4,offset = 0,
        DTOutput("tableDbNetwork")
        ),
  column(8,offset = 0,
         hidden(actionButton("loadNetworkExplorer", "Load Circuit to GeneVyuha", icon = NULL, width = NULL)),
         hidden(actionButton("loadNetworkRACIPE", "Load Circuit to RACIPE", icon = NULL, width = NULL)),
         visNetworkOutput("plotDbNetwork")
  )
  ),
    plotOutput("plotDbNetworkExprs"),
hidden(downloadButton('downloadDbData', 'Download Data')),
hr(),
hr()
#print("~~~Disclaimer~~~~")
  )
