source("modules.R")
racipe <-
  tabPanel("RACIPE",
           useShinyjs(),

############## RACIPE SIMULATIONS ##########################
  fluidRow(
    column(5, offset=0,
         fluidRow(
           column(9, offset=1,
           ( numericInput(inputId = "numModels", "Number of Models",  min = 1, 
                          max = 5000, value = 500.0)))),
         fluidRow(
           column(9, offset=1,
                  numericInput(inputId = "racipeNClusters", 
                               "Expected Number of Clusters",  min = 2, step = 1,
                               max = 10, value = 3))),
          fluidRow(
            column(9, offset=1,
             ( numericInput(inputId = "parameterRange", "Parameter Range",  
                            min = 1, max = 100, value = 100)))),
           fluidRow(
             column(9, offset=1,
           ( numericInput(inputId = "simTimeRacipe", "Simulation Time", 
                          min = 1, max = 5000, value = 50.0)))),
           fluidRow(
             column(9, offset=1,
           ( numericInput(inputId = "stepSizeRacipe", "Integration Time Step",
                          min = 0.001, max = 0.9, value = 0.05)),
     #      checkboxInput("emailRacipe", "Email data download link",
       #                  value = FALSE),
           withBusyIndicatorUI(actionButton("simulateRacipe", "Simulate", 
                                               class = 'btn-primary',
    style="color: #fff; background-color: #32CD32; border-color: #2e6da4",
    title = "Simulate the circuit with given parameters"))))),
  column(6, offset=0,
  visNetworkOutput("racipeCircuit"))
),
# bsAlert("racipeAlert"),
       fluidRow(
       htmlOutput("racipeDeterministicText")),

       fluidRow(

         shinyjs::hidden(plotOutput("racipePca")),
         shinyjs::hidden(plotOutput("racipeHeatmap"))

),
fluidRow(
column(3, offset=8,
       shinyjs::hidden(actionButton("validateRacipe", "Load for Validation", 
                           style="color: #fff; background-color: #32CD32; 
                           border-color: #2e6da4", title = "Use this 
                           simulated data for comparison with experimental
                           data in the validation tab")))
),
################### RACIPE DATA DOWNLOAD #########################

fluidRow(
  column(3, offset=1,
         fluidRow(
           hidden(downloadButton('downloadRacipeData', 'Download Data'))),
         fluidRow(
           hidden(radioButtons("downloadRacipeDataType", "Format",
                               c("RDS" = "RDS","Text" = "txt") , 
                               selected = "RDS",
                               inline = TRUE)))),

  column(3, offset=0,
         hidden(actionButton("saveRacipeData", "Upload Options"
# style="color: #fff;background-color: #337ab7; border-color: #2e6da4"
)))


),

fluidRow(
  column(3, offset = 1,
         shinyjs::hidden(textInput("uploadToDatabaseUI_name_Racipe", "Name", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_lab_Racipe", "Lab", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_contact_Racipe", 
                            "Contact", "")))
),
fluidRow( column(10, offset = 1,
                 shinyjs::hidden(   textInput("uploadToDatabaseUI_title_Racipe", 
                                     "Title", "")))
),
fluidRow( column(10, offset = 1,
                 shinyjs::hidden(   textInput("uploadToDatabaseUI_abstract_Racipe",
                                     "Abstract", "")))
),
fluidRow(
  column(3, offset = 1,
         shinyjs::hidden( textInput("uploadToDatabaseUI_url_Racipe", "URL", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_pubMedIds_Racipe", 
                            "PubMed ID", "")))
),

fluidRow(
  column(1, offset=1,
         withBusyIndicatorUI(shinyjs::hidden(actionButton("uploadRacipeData", 
                                                          "Upload",
style="color: #fff;background-color: #337ab7; border-color: #2e6da4")))),
  column(3, offset=0,
         shinyjs::hidden(htmlOutput("fileRacipeData")))
),
           hr(),
################# PARAMETERIC ANALYSIS ###########################
       fluidRow(
         column(5, offset=4,
                shinyjs::hidden(  actionButton("parametricAnalysisRacipe", 
                                      "Parametric Analysis", 
style="color: #fff;background-color: #337ab7; border-color: #2e6da4", 
title = "Display the distributions when the parameter values are limited to 
a given percentage of the original distribution.")))
         # hidden(tags$h5(id = "paDescrpt","Use the sliders to control the range of one or more 
         #                parameters and observe the resulting change in 
         #                distribution of models in different clusters"))
       ),
       fluidRow(
         column(3,
                shinyjs::hidden( uiOutput("filteredOutputRacipe")),
                shinyjs::hidden(uiOutput("filterSliderRacipe"))
),
column(3,
       hidden(uiOutput("filteredOutputRacipe2")),
       shinyjs::hidden( uiOutput("filterSliderRacipe2"))),
column(3,

       shinyjs::hidden(  uiOutput("filteredOutputRacipe3")),
       shinyjs::hidden(uiOutput("filterSliderRacipe3"))
)),


           fluidRow(
          #   column(8,
             shinyjs::hidden(    plotOutput("racipePcaFiltered")),
          #   ),
          #   column(4,
          shinyjs::hidden(  plotOutput("racipeHeatmapFiltered"))
           #  )
           ),

hr(),
################# STOCHASTIC RACIPE ##############################
fluidRow(
  column(5, offset=4,
         shinyjs::hidden(actionButton("stochasticRacipe", "Stochastic RACIPE", 
                       style="color: #fff; background-color: #337ab7; 
                       border-color: #2e6da4", title = "Displays stochastic
                simulation options")))
),
fluidRow(
  column(3,
         shinyjs::hidden(radioButtons("sRacipeOption", "Stochastic Simulation Type:",
                      c("Constant Noise" = "constantNoise",
                        "Annealing" = "annealing")))
  ),
  column(3, offset=0,
         shinyjs::hidden(  sliderInput("sRacipeNoise", "Noise Level",step = 1,  min = 1, 
                              max = 20, value = 0))),

  column(5, offset=0,
         br(),
     #    shinyjs::hidden(checkboxInput("emailSRacipe", "Email data download link", value = FALSE)),
         withBusyIndicatorUI(     shinyjs::hidden(  actionButton("simulateSRacipe", 
      "Perform Stochastic Simulations",  class = 'btn-primary',
                               style="color: #fff; background-color: #32CD32; 
                           border-color: #2e6da4"))))
),
fluidRow(
  column(6,
         shinyjs::hidden(plotOutput("sRacipePcaDet"))
  ),
column(6,
       shinyjs::hidden(plotOutput("sRacipePca"))
)),
fluidRow(
  column(6,
hidden(plotOutput("sRacipeHeatmapDet"))),
column(6,
       shinyjs::hidden(plotOutput("sRacipeHeatmap")))),


################### SRACIPE DATA DOWNLOAD #########################

fluidRow(
  column(3, offset=1,
         fluidRow(
           shinyjs::hidden(downloadButton('downloadSRacipeData', 'Download Data'))),
         fluidRow(
           shinyjs::hidden(radioButtons("downloadSRacipeDataType", "Format",
                               c("RDS" = "RDS","Text" = "txt") , 
                               selected = "RDS",
                               inline = TRUE)))),

  column(3, offset=0,
         shinyjs::hidden(actionButton("saveSRacipeData", "Upload Options"
# style="color: #fff;background-color: #337ab7; border-color: #2e6da4"
)))

),

fluidRow(
  column(3, offset = 1,
         shinyjs::hidden(textInput("uploadToDatabaseUI_name_sRacipe", "Name", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_lab_sRacipe", "Lab", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_contact_sRacipe", 
                            "Contact", "")))
),
fluidRow( column(10, offset = 1,
                 shinyjs::hidden(   textInput("uploadToDatabaseUI_title_sRacipe",
                                     "Title", "")))
),
fluidRow( column(10, offset = 1,
                 shinyjs::hidden(   textInput("uploadToDatabaseUI_abstract_sRacipe",
                                     "Abstract", "")))
),
fluidRow(
  column(3, offset = 1,
         shinyjs::hidden( textInput("uploadToDatabaseUI_url_sRacipe", "URL", ""))),
  column(3, offset = 0,
         shinyjs::hidden(  textInput("uploadToDatabaseUI_pubMedIds_sRacipe", 
                            "PubMed ID", "")))
),

fluidRow(
  column(1, offset=1,
         withBusyIndicatorUI(
           shinyjs::hidden(actionButton("uploadSRacipeData", "Upload",
style="color: #fff;background-color: #337ab7; border-color: #2e6da4")))),
  column(3, offset=0,
         shinyjs::hidden(htmlOutput("fileSRacipeData")))
),
#downloadButton('downloadCNData', 'Download Constant Noise Simulation Data'),



#downloadButton('downloadAnnealData', 'Download Annealing Simulation Data')

#
# numericInput("KdReduction", "Decrease in production upon knockdown (%)"),
#            fluidRow(
#              column(5, offset=4,
#                     actionButton("simulateRacipe", "Plot Knockdown Statistics"
# , style='padding:10px; font-size:100%'))
#            )
hr()
  )

