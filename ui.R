    dHeader <- dashboardHeader(title="Leveraged ETF")
    
    dSidebar <- dashboardSidebar(
                                
                                selectInput(
                                  "Frequency",
                                  label = h3("Frequency"),
                                  choices = list("Daily" = "Daily", "Weekly" = "Weekly", "Monthly" = "Monthly", "Quarterly" = "Quarterly", "Yearly" = "Yearly"),
                                  selected = "Yearly"),
                                
                                selectInput(
                                  "Task",
                                  label = h3("Task"),
                                  choices = list("Back Testing" = "backTesting", "Monte Carlo Simulation" = "monteCarloSimulation"),
                                  selected = "Back Testing"),
                                
                                conditionalPanel("input.Task=='backTesting'",
                                                 selectInput("displayedOutputs",label=h3("Results"),
                                                             choices=list("Tables"="tables","Summary Statistics"="summaryStatistics","Charts"="charts"),
                                                             selected="Tables")),
                                
                                conditionalPanel("input.Task=='monteCarloSimulation'",
                                                 selectInput("displayedOutputs2",label=h3("Results"),
                                                             choices=list("Simulations"="simulations","Simulation Graph"="simulationGraph"),
                                                             selected="Simulations"),
                                                 numericInput("NumberOfSimulations", label = h3("Number of Simulations"), value = 1),
                                                 numericInput("StartingPoint", label = h3("Initial Value"), value = 1)

               ))
    
    dBody <- dashboardBody(
        conditionalPanel("input.Task=='backTesting'",
                         conditionalPanel("input.displayedOutputs=='tables'",tableOutput('Results')),
                         conditionalPanel("input.displayedOutputs=='charts'",plotOutput('Plots1',height = 1000))
                        ),
        conditionalPanel("input.Task=='monteCarloSimulation'",
                         conditionalPanel("input.displayedOutputs2=='simulations'",tableOutput('MCSResults')),
                         conditionalPanel("input.displayedOutputs2=='simulationGraph'",plotOutput('Plots2',height = 1000))
                         
                         )
        
                                        
                                      
                                    
                            )
  
    
    
    ui <-     dashboardPage(dHeader,dSidebar,dBody)
