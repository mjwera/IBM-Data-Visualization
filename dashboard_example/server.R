# Import libraries
library(shiny)
library(ggplot2)

# Define server logic
shinyServer(function(input, output) {
  # TASK 6: Histogram 
  output$p1 <- renderPlot({
    # Base ggplot object
    p <- ggplot(mtcars, aes(x = ...)) +
      labs(y = "Number of Cars", title = paste("Trend of ", ...))
    
    # Based on radio button input, change histogram fill
    if (input$hist_fill == "default") {
      p + geom_histogram(bins = ...)
    }
    else {
      p + geom_histogram(bins = ..., fill = ...)
    }
  })
  
  # TASK 7: Boxplot
  output$p2 <- renderPlot({
    ggplot(mtcars, aes(y = ...)) + 
      geom_boxplot() + 
      labs(title = paste("How",..., "value is spread")) +
      coord_flip()
  })
  
  # TASK 8: Bar chart
  output$p3 <- renderPlot({
    ggplot(data = mtcars, aes(x = factor(...), fill = factor(...))) +
      geom_bar() +
      labs(x = ..., title = paste("Trend of", ...))
  })
  
  # TASK 9: Scatter plot
  output$p4 <- renderPlot({
    ggplot(mtcars, aes(x = ..., y = wt, color = factor(...))) + 
      geom_point(size = 3) +
      labs(title = paste("Distribution of", ..., "with respect to Weight"))
  })
})
