source("modules.R")
modelExplorer <-  tabPanel(
  "Model Explorer",        tags$em("
'GeneVyuha' tab simulates a circuit with a specific parameter set. 
        Once the circuit is loaded from the 'Circuit' or 'Database' tab, a 
        random set of parameters is generated. If the circuit from the 
database has its own parameter set, that set is used. The parameter set contains
two paramters for each gene and three parameters for each interactions. 
The gene parameters are production (G_'gene') and degradation (k_'gene') rate 
whereas the parameters for each interaction ('source'_'target') are 'threshold'
(TH_), 'hill coefficient of cooperativity' and 'fold change'. 
These parameters can be modified using the dropdown box given below. 
        The default value of the parameter is dispalyed when a parameter is 
        selected. Edit the 'value' and click 'Update' to modify this value. 
        Repeat  these steps if other parameters are to be modified. Other 
        simulation paramters can also be modified."),
  br(),
  useShinyjs(),
  fluidRow(
    column(6, offset = 0,
           uiOutput("modelParams"),
           fluidRow(
             column(4, offset = 0,
                    style = "margin-top:-1em",
                    uiOutput("newModelParamValue")),
             column(3, offset = 0, style = "margin-top: 10px",
                    actionButton("updateGvParam",   "Update",  width = "100%",
  style="color: #fff;background-color: #337ab7; border-color: #2e6da4",
  title = "Modify the parameters using this button. 
  Enter the new value in the 'value' input and 'update'. ")
  )
  ),
  br(),
  shinyBS::tipify(numericInput(inputId = "simTimeExplorer", 
                               "Simulation Time",  min = 1, 
               max = 5000, value = 50.0),"Simulation time after which the 
               gene expression values will be recorded. 
               Use a larger value if you 
expect longer transient dynamics."),
  shinyBS::tipify(numericInput(inputId = "stepSizeExplorer",
                               "Integration Time Step", min = 0.001, max = 0.9,
                               value = 0.1),
                  "Time step to be used in numerical integration methods."),
                                      
                                     numericInput("noiseLevel", 
   "Noise",step = 0.1,  min = 0, max = 100, value = 0),
                                       
                                      br(),
#  checkboxInput("emailGv", "Email data download link", value = FALSE),
  
  withBusyIndicatorUI(

                                    actionButton("simulateGv", "Simulate",
                                                 class = "btn-primary",
   style="color: #fff; background-color: #32CD32; border-color: #2e6da4",
   title = "Simulate the circuit using the above parameter values. 
    Change the circuit using the Circuit tab."))),
                             column(6, offset = 0,
                                    visNetworkOutput("circuitGv")
                             )
                             ),
                           fluidRow(
                             hidden( plotOutput("GvTS"))
                             ),

hr(),
fluidRow(
  column(3, offset=1,
         fluidRow(
           hidden(downloadButton('downloadMEData', 'Download Data'))),
         fluidRow(
           hidden(radioButtons("downloadMEDataType", "Format",
                               c("RDS" = "RDS","Text" = "txt") , 
                               selected = "RDS",
                               inline = TRUE)))),

  column(3, offset=0,
         hidden(actionButton("saveMEData", "Upload Options")))

),
fluidRow(
  column(3, offset = 1,
hidden(textInput("uploadToDatabaseUI_name_Explorer", "Name", ""))),
  column(3, offset = 0,
 hidden(  textInput("uploadToDatabaseUI_lab_Explorer", "Lab", ""))),
  column(3, offset = 0,
hidden(  textInput("uploadToDatabaseUI_contact_Explorer", "Contact", "")))
),
fluidRow( column(10, offset = 1,
hidden(   textInput("uploadToDatabaseUI_title_Explorer", "Title", "")))
),
fluidRow( column(10, offset = 1,
hidden(   textInput("uploadToDatabaseUI_abstract_Explorer", "Abstract", "")))
),
fluidRow(
  column(3, offset = 1,
 hidden( textInput("uploadToDatabaseUI_url_Explorer", "URL", ""))),
  column(3, offset = 0,
 hidden(  textInput("uploadToDatabaseUI_pubMedIds_Explorer", "PubMed ID", "")))
),

fluidRow(
  column(1, offset=1,
         withBusyIndicatorUI( hidden(actionButton("uploadMEData", "Upload",
                                                  class = 'btn-primary')))),
  column(3, offset=0,
         hidden(htmlOutput("fileSaveDatabase1")))
),
hr(),
# ############## Bifurcations ##############################################

fluidRow(

  column(4, offset=4,
         hidden(actionButton("bifurcationExplorer", "Parameter Perturbation", 
style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
),

br(),
fluidRow(
  column(3, offset=0,
         hidden(uiOutput("modelParamsBif")),
         hidden(uiOutput("modelParamBifMin")),
         hidden( uiOutput("modelParamBifMax")),
         hidden( numericInput("modelNumBifurs", "Simulation Points", 300,
                              min = 50, max = 5000, step = 50)),
         
         withBusyIndicatorUI( hidden(  actionButton("bifurcationME", "Simulate",
                                                    class = 'btn-primary',
style="color: #fff; background-color: #32CD32; border-color: #2e6da4",
title = "Simulate circuit to generate the gene expressions for models with
varying selected parameter.")))
         ),
  column(9, offset=0,
         
         hidden(  plotOutput("modifiedBifME")),
         hidden(downloadButton('downloadBifData', 'Download Bifurcation Data'))
)),

hr(),
hr()
)
