# Import libraries
library(shiny)
library(ggplot2)

# Define server logic
shinyServer(function(input, output) {
  # TASK 6: Histogram 
  output$p1 <- renderPlot({
    # Base ggplot object
    p <- ggplot(mtcars, aes(x = !!input$continuous_variable)) + #"!!" is a special function from rlang that interprets it as the variable
      labs(y = "Number of Cars", title = paste("Trend of ", input$continuous_variable)) #"!!" is not needed here
    
    # Based on radio button input, change histogram fill from default to dodgerblue 3
    if (input$hist_fill == "default") {
      p + geom_histogram(bins = input$bins)
    }
    else {
      p + geom_histogram(bins = input$bins, fill = "dodgerblue3")
    }
  })
  
  # TASK 7: Boxplot
  output$p2 <- renderPlot({
    ggplot(mtcars, aes(y = !!input$continuous_variable)) + 
      geom_boxplot() + 
      labs(title = paste("How",input$continuous_variable, "value is spread")) +
      coord_flip() #this displays boxplot horizontally instead of vertically
  })
  
  # TASK 8: Bar chart
  output$p3 <- renderPlot({
    ggplot(data = mtcars, aes(x = factor(!!input$categorical$variable), fill = factor(!!input$categorical_variable))) +
      geom_bar() +
      labs(x = input$categorical_variable, title = paste("Trend of", input$categorical_variable))
  })
  
  # TASK 9: Scatter plot
  output$p4 <- renderPlot({
    ggplot(mtcars, aes(x = !!input$continuous_variable, y = wt, color = factor(!!input$categorical_variable))) + 
      geom_point(size = 3) +
      labs(title = paste("Distribution of", input$continuous_Variable, "with respect to Weight"))
  })
})
