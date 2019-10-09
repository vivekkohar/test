###########################################
# Evaluate Server
###########################################

############## Show Hide Options #####################################
observeEvent(input$evaluate,{
  

  
})

# Array to hold reactive variables 
evalVariables = reactiveValues()
# Limit the maximum number of models to 5000.
evalVariables$pValue <- reactive({
  if((input$evalPValue > 1)|(input$evalPValue < 0)) return(.05)
  else return(input$evalPValue)
})
#evalVariables$simExp <- reactive()
# Limit maximum time to 5000.
evalVariables$nClust <- reactive({
  if(input$evalNClust > 8) return(8)
  else return(input$evalNClust)
})
# Use maximum and minimum limits on the initial step size
evalVariables$evalNPermut <- reactive({
  if(input$stepSizeExplorer > 10000) return(10000)
  else return(input$evalNPermut)
})

# Update the network if [Load Circuit] button is clicked from [Load File] tab
observeEvent(input$uploadRefExp,{
  refExp <-  input$fileRefExp
  if(is.null(refExp)) {
    return(NULL)
  }
  evalVariables$refExp <- reactive(as.matrix(read.table(refExp$datapath,sep="", header =TRUE, stringsAsFactors = FALSE)))
   # rownames(uploadedData) <- uploadedData[,1]
   # uploadedData <- uploadedData[,2:dim(uploadedData)[2]]
  # flush the outputs if new data is uploaded
  output$evalRefHeatmap <- renderPlot({
    if(is.null(evalVariables$refExp())) return()
    plotData <- evalVariables$refExp()
    #gplots::heatmap.2(evalVariables$refExp(), 
                      #   hclustfun = function(x) hclust(x,method = 'ward.D2'), 
                      #    distfun=function(x) as.dist((1-cor(t(x), method = "spear"))/2), 
    #                  trace="none")
   # nClusters <- evalVariables$nClust()
  .sracipePlotHeatmap(plotData, dendrogram = "none" 
                      #assignedClusters = evalVariables$refClust() 
                      )
  })
})

  # Update the network if [Load Circuit] button is clicked from [Load File] tab
  observeEvent(input$uploadRefClust,{
    refClust <-  input$fileRefClust
    if(is.null(refClust)) {
      return(NULL)
    }
   evalVariables$refClust <- reactive(as.character(read.table(refExp$datapath,sep="", header =FALSE, stringsAsFactors = FALSE)))
 })

output$evalSimHeatmap <- renderPlot({
  if(is.null(evalVariables$simExp())) return()
  plotData <- evalVariables$simExp()
  gplots::heatmap.2(plotData, col=plotColor, 
                    hclustfun = function(x) hclust(x,method = 'ward.D2'), 
                    distfun=function(x) as.dist((1-cor(t(x), method = "spear"))/2), 
                    trace="none", ...)
  #.sracipePlotHeatmap(plotData)
})

