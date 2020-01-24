

server <- function(input, output, session) 
    {
   
        chosenFrequency <- reactive({input$Frequency})    
        output$Results <- renderTable({as.data.frame(BackTesting(chosenFrequency())[1])})
    
        output$Plots1 <- renderPlot({BackTesting(chosenFrequency())[[2]]})
    
        numberOfSimulations <- reactive({input$NumberOfSimulations})
        startingPoint <- reactive({input$StartingPoint}) 
        
        output$MCSResults <- renderTable({MCS(chosenFrequency(),numberOfSimulations(),startingPoint())[[1]]})
        output$Plots2 <- renderPlot({MCS(chosenFrequency(),numberOfSimulations(),startingPoint())[[2]]})
        
    }


