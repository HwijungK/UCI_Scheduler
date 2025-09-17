library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "UCI Schedule Crafter"
  ),
  dashboardSidebar(
    
  ),
  dashboardBody(
    box(
      width = 10,
      uiOutput("slider"),
      actionButton("dec.plot.index", "Prev"),
      actionButton("inc.plot.index", "Next")
      
    ),
    box(
      width = 10,
      height =800,
      plotOutput("cal.plot", height = 700),
      textOutput("debug")
    )
  )
)

i <- reactiveVal(5)
server <- function(input, output, session) {
  observeEvent(input$inc.plot.index, {
    old_val <- i()
    i(min(old_val + 1, length(cal.plots)))  # cap at max
    #cat("Next Button Clicked! old i:", old_val, " new i:", i(), "\n")
  })
  
  observeEvent(input$dec.plot.index, {
    old_val <- i()
    i(max(old_val - 1, 1))  # cap at min
    #cat("Previous Button Clicked! old i:", old_val, " new i:", i(), "\n")
  })
  
  observeEvent(input$plot.index.slider, {
    i(input$plot.index.slider)
  })
  output$slider <- renderUI({
    sliderInput("plot.index.slider",
                label = "Choose Index",
                min = 1,
                max = length(cal.plots),
                value = i(),
                step = 1,
                animate = F,
                width = '80%')
  })
  
  output$cal.plot <- renderPlot({
    cal.plots[[i()]]
  })
  output$debug <- renderText({i()})
}

shinyApp(ui, server)
