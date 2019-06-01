library(shiny)
library(fractal)
library(plotly)

# Define a function to parse input and evaluate
evalIn <- function(x) {
  return(eval(parse(text = x)))
}

# Generate discrete sequence of time
t <- seq(0, 40, .05)

ui <- fluidPage(
  titlePanel("DelayExplore"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        withMathJax("Interactively view delay embeddings in $\\mathbb{R}^3$."),
        tags$br(), tags$br(),
        "To begin, try rotating the point-cloud by dragging."
      ),
      br(),
      
      textInput(inputId = "fun", 
                label = "Function: $f(t)$", 
                value = "cos(t) + cos(pi*t)", 
                placeholder = "cos(t) + cos(pi*t)"),
      
      sliderInput(inputId = "delay",
                  label = "Delay: $\\tau$",
                  min = 1,
                  max = 200, 
                  value = 14,
                  step = 1),
      
      hr(),
      
      plotlyOutput(outputId = "line")
    ),
    
    mainPanel(
      plotlyOutput(outputId = "point")
    )
  )
)

server <- function(input, output) {

  output$point <- renderPlotly({
    
    # Create the delay embedding
    d.em <- embedSeries(evalIn(input$fun), tlag = input$delay, dim = 3)
    d.em <- as.matrix(d.em)
    d.em <- as.data.frame(d.em)
    names(d.em) <- c("t", "t.tau", "t.2tau")
    
    plot_ly(d.em, x = ~t, y = ~t.tau, z = ~t.2tau, 
            type = "scatter3d", mode = "markers", height = 800,
            marker = list(color = ~t.2tau, 
                          size = 3, 
                          colorscale = "Viridis", 
                          showscale = F)) %>% 
      layout(
        scene = list(xaxis = list(title = "f(t)"),
                     yaxis = list(title = paste0("f(t + ", input$delay, ")")),
                     zaxis = list(title = paste0("f(t + 2*", input$delay, ")")))
      )
    
  })
  
  output$line <- renderPlotly({
    
    f <- evalIn(input$fun)
    
    p <- plot_ly(x = t, y = f, 
                 type = 'scatter', mode = 'lines', 
                 height = 100) %>% 
      layout(
        xaxis = list(title = "t"),
        yaxis = list(title = "f(t)"),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent'
      )
  })
  
}

shinyApp(ui, server)