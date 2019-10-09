evaluate <-  tabPanel("Evaluate",
                      fluidRow(
                        
                        column(3, offset = 0,
                               tags$h4("Reference Expression"),
                                    fluidRow(
                                      tags$h5("Reference Data")
                                      ),
                               fluidRow(
                                 column(6, offset = 0, 
                                        fileInput(("fileRefExp"),label = NULL)
                                        ),
                                 column(3, offset = 0,
                                        actionButton("uploadRefExp", "Upload")
                                        )
                                 ),
                               tags$h5("Cluster Information"),
                               fluidRow(
                                 column(6, offset = 0,
                                        fileInput("fileRefClust",label = NULL)
                                        ),
                                 column(3, offset = 0,
                                        actionButton("uploadRefClust", "Upload")
                                        )
                                 )
            
                               
                                    ),
                        column(4, offset=1,
                               plotOutput("evalRefHeatmap")
                        ),
                           column(4,offset = 0,
                                   plotOutput("evalSimHeatmap")
                        )
                               

                        ),
                      fluidRow(
                        column(2, offset=1,
                        numericInput(inputId = "evalPValue", "p-value",  min = 0, max = 1, value = 0.05)
                        ),
                        column(2, offset=0,
                               numericInput(inputId = "evalNClust", "Number of Clusters",  min = 2, max = 8, value = 3)
                        ),
                        column(2, offset=0,
                               numericInput(inputId = "evalNPermut", "Number of permutations",  min = 100, max = 10000, value = 1000)
                        )
                      )
                      
                      
                    #  fluidRow(
                    #    plotOutput("evalRefHeatmap")
                    #  )
                      
)

                           