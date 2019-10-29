############## Network Input ##################################################

circuit <-  tabPanel(
  "Circuit", tags$em(
  "
The network/circuit to be simulated can be specified here.
There are multiple ways to do so. As an example, we have
loaded the canonical toggle switch circuit as the default circuit. 
It can be modified either by loading the circuit from a file containing 
three columns, 'Source', 'Target', 'Interaction' (1 - activation, 2 - inhibition
) separated by space and each row
specifying an interaction. One can also 'add', 'delete', 'undo delete'
interactions
in the table below. Clicking the 'Load Circuit' button will load the circuit 
and display it in the right panel. Database tab can be used to specify a circuit 
if one of the circuits from the database is to be loaded. Use 'Circuit Name' to
provide a specific name to your circuit.
'Download Circuit' will download the loaded circuit as a text file."),
  hr(),
  fluidRow(
    column(3,offset = 0,
    uiOutput('undoUI')),
    column(3,offset = 0,
    actionButton(
      "addInteraction","Add Interaction", 
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
    ),
  fluidRow(
    DT::dataTableOutput("circuitTable"),
    # uiOutput("circuitTable"),
    shinyBS::bsTooltip(
      "circuitTable", "Circuit interactions.",
      placement = "bottom", trigger = "hover", options = NULL),
    fileInput(("circuitFile"),label = "Choose Circuit File"),
    shinyBS::bsTooltip("circuitFile", "Input the circuit using a file.",
                       placement = "bottom", trigger = "hover", options = NULL),
    textInput(("circuitName"), "Circuit Name", "Circuit1"),
    shinyBS::bsTooltip(
    "circuitName", "Name of the circuit which will be used as
    annotation for RacipeSE object", placement = "bottom", 
    trigger = "hover", options = NULL),
    downloadButton(('downloadCircuit'), 'Download Circuit'),
    shinyBS::bsTooltip(
      "downloadCircuit", "Download the circuit as a text file.", 
      placement = "bottom", trigger = "hover", options = NULL)

),

fluidRow(
  column(3,offset = 5,
         actionButton(
           "updateCircuit", "Load Circuit",icon("thumbs-up"),
           style="color: #fff; background-color: #32CD32; 
           border-color: #2e6da4")
         )
  ),
  shinyBS::bsTooltip(
  "updateCircuit", "If the interactions look good, you can 
  Load the circuit for further analysis. 
  It will update the circuit figures.", placement = "bottom", 
  trigger = "hover", options = NULL),

fluidRow(  visNetwork::visNetworkOutput("circuit")),

hr()
#                           
)
