source("modules.R")
racipe <-
  tabPanel("RACIPE",
           useShinyjs(),

######## IMPORT NETWORK ########################
           fluidRow(
             column(3, offset=0,
                    br(),
                    actionButton("importCircuit", "Import Circuit from GeneVyuha", style='padding:10px; font-size:100%')),
   #          column(3, offset=0, uiOutput("storedTpoFiles")),
    #         column(3, offset=0,
                    br()
     #               actionButton("importCircuitFrmDatabase", "Import Circuit from Database", style='padding:10px; font-size:100%'))

           ),

       #    shinyLoadNetworkUI("shinyRacipeNetwork"),
       fluidRow(
         htmlOutput("circuitMsg"),
       visNetworkOutput("racipeNetwork")),
  #     column(1, offset = 0,    img(src='JAX.gif', align = "right"))),

  hr(),

############## RACIPE SIMULATIONS ##########################
  fluidRow(

    column(2, offset=0,
           hidden( numericInput(inputId = "numModels", "Number of Models",  min = 1, max = 5000, value = 500.0))),
    column(2, offset=0,
           hidden( numericInput(inputId = "parameterRange", "Parameter Range",  min = 1, max = 100, value = 100))),
    column(2, offset=0,
           hidden( numericInput(inputId = "simTimeRacipe", "Simulation Time",  min = 1, max = 5000, value = 50.0))),
    column(2, offset=0,
           hidden( numericInput(inputId = "stepSizeRacipe", "Integration Time Step",  min = 0.001, max = 0.9, value = 0.05)))
  ),
  hr(),


  fluidRow(
             column(5, offset=4,
                    hidden(actionButton("simulateRacipe", "Deterministic RACIPE", style='padding:10px; font-size:100%')))
           ),


       fluidRow(
       htmlOutput("racipeDeterministicText")),

       fluidRow(
           column(8,

           hidden(plotOutput("racipeHeatmap"))
           ),
           column(4,
           hidden(plotOutput("racipePca"))
           )),
hr(),
################### RACIPE DATA DOWNLOAD #########################

fluidRow(
  column(3, offset=1,
         fluidRow(
           hidden(downloadButton('downloadDataRacipe', 'Download Data'))),
         fluidRow(
           hidden(radioButtons("downloadDataRacipeType", "Format",
                               c("RDS" = "RDS","Text" = "txt") , selected = "RDS",
                               inline = TRUE)))),

  column(3, offset=0,
         hidden(actionButton("saveDataRacipe", "Upload Options")))

),

fluidRow(
  column(3, offset = 1,
         hidden(textInput("uploadToDatabaseUI_name_Racipe", "Name", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_lab_Racipe", "Lab", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_contact_Racipe", "Contact", "")))
),
fluidRow( column(10, offset = 1,
                 hidden(   textInput("uploadToDatabaseUI_title_Racipe", "Title", "")))
),
fluidRow( column(10, offset = 1,
                 hidden(   textInput("uploadToDatabaseUI_abstract_Racipe", "Abstract", "")))
),
fluidRow(
  column(3, offset = 1,
         hidden( textInput("uploadToDatabaseUI_url_Racipe", "URL", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_pubMedIds_Racipe", "PubMed ID", "")))
),

fluidRow(
  column(1, offset=1,
         hidden(actionButton("uploadDataRacipe", "Upload"))),
  column(3, offset=0,
         hidden(htmlOutput("fileDataRacipe")))
),
           hr(),
################# PARAMETERIC ANALYSIS ###########################
       fluidRow(
         column(5, offset=5,
                hidden(  actionButton("parametricAnalysisRacipe", "Parametric Analysis", style='padding:10px; font-size:100%'))),
         hidden(tags$h5("Use the sliders to control the range of one or more parameters and observe the resulting change in distribution of models in different clusters"))
       ),
       fluidRow(
         column(3,
        hidden( uiOutput("filteredOutputRacipe")),
           hidden(uiOutput("filterSliderRacipe"))
),
column(3,
       hidden(uiOutput("filteredOutputRacipe2")),
              hidden( uiOutput("filterSliderRacipe2"))),
column(3,

       hidden(  uiOutput("filteredOutputRacipe3")),
       hidden(uiOutput("filterSliderRacipe3"))
)),


           fluidRow(
             column(8,
                    hidden(  plotOutput("racipeHeatmapFiltered"))
             ),
             column(4,
                    hidden(    plotOutput("racipePcaFiltered"))
             )
           ),
hr(),

################# STOCHASTIC RACIPE ##############################
fluidRow(
  column(5, offset=4,
         hidden(actionButton("stochasticRacipe", "Stochastic RACIPE", style='padding:10px; font-size:100%')))
),
fluidRow(
  column(3,
         hidden(radioButtons("sRacipeOption", "Stochastic Simulation Type:",
                      c("Constant Noise" = "constantNoise",
                        "Annealing" = "annealing")))
  ),
  column(3, offset=0,
         hidden(  sliderInput("sRacipeNoise", "Noise Level",step = 1,  min = 1, max = 20, value = 0))),

  column(5, offset=0,
         br(),
         hidden(  actionButton("simulateSRacipe", "Perform Stochastic Simulations", style='padding:10px; font-size:100%')))
),
fluidRow(
  column(8,
hidden(plotOutput("sRacipeHeatmap"))),
column(4,
       hidden(plotOutput("sRacipePca"))
)),
hr(),

################### SRACIPE DATA DOWNLOAD #########################

fluidRow(
  column(3, offset=1,
         fluidRow(
           hidden(downloadButton('downloadDataSRacipe', 'Download Data'))),
         fluidRow(
           hidden(radioButtons("downloadDataSRacipeType", "Format",
                               c("RDS" = "RDS","Text" = "txt") , selected = "RDS",
                               inline = TRUE)))),

  column(3, offset=0,
         hidden(actionButton("saveDataSRacipe", "Upload Options")))

),

fluidRow(
  column(3, offset = 1,
         hidden(textInput("uploadToDatabaseUI_name_sRacipe", "Name", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_lab_sRacipe", "Lab", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_contact_sRacipe", "Contact", "")))
),
fluidRow( column(10, offset = 1,
                 hidden(   textInput("uploadToDatabaseUI_title_sRacipe", "Title", "")))
),
fluidRow( column(10, offset = 1,
                 hidden(   textInput("uploadToDatabaseUI_abstract_sRacipe", "Abstract", "")))
),
fluidRow(
  column(3, offset = 1,
         hidden( textInput("uploadToDatabaseUI_url_sRacipe", "URL", ""))),
  column(3, offset = 0,
         hidden(  textInput("uploadToDatabaseUI_pubMedIds_sRacipe", "PubMed ID", "")))
),

fluidRow(
  column(1, offset=1,
         hidden(actionButton("uploadDataSRacipe", "Upload"))),
  column(3, offset=0,
         hidden(htmlOutput("fileDataSRacipe")))
)
#downloadButton('downloadCNData', 'Download Constant Noise Simulation Data'),



#downloadButton('downloadAnnealData', 'Download Annealing Simulation Data')

#
# numericInput("KdReduction", "Decrease in production upon knockdown (%)"),
#            fluidRow(
#              column(5, offset=4,
#                     actionButton("simulateRacipe", "Plot Knockdown Statistics", style='padding:10px; font-size:100%'))
#            )

  )

