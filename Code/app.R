library(shinydashboard)
library(bslib)
library(magrittr)

ui <- dashboardPage(
  dashboardHeader(
    title = "UCI Schedule Crafter"
  ),
  dashboardSidebar(
    
  ),
  dashboardBody(
    box(
      # Course Selection
      width = 10,
      textInput("courses.text", "Search Courses"),
      input_task_button("courses.submit", "GO"),
      textOutput("courses.output")
    ),
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
  # sched list selection
  observeEvent(input$inc.plot.index, {
    old_val <- i()
    i(min(old_val + 1, length(cal.plots())))
  })
  
  observeEvent(input$dec.plot.index, {
    old_val <- i()
    i(max(old_val - 1, 1))
  })
  
  observeEvent(input$plot.index.slider, {
    i(input$plot.index.slider)
  })
  output$slider <- renderUI({
    sliderInput("plot.index.slider",
                label = "Choose Index",
                min = 1,
                max = length(cal.plots()),
                value = i(),
                step = 1,
                animate = F,
                width = '80%')
  })
  output$cal.plot <- renderPlot({
    cal.plots()[[i()]]
  })
  output$debug <- renderText({
    as.character(courses.df()[[1]])
    })
  
  # course selection
  courses.df <- eventReactive(input$courses.submit, ignoreNULL = FALSE, {
    c.list <- input$courses.text %>%
      strsplit(",[ ]*") %>%
      unlist() %>%
      sub("^(.*) (.*)$", replacement = "\\1;\\2", .) %>%
      strsplit(';')
    
    do.call(args = c.list, what = rbind) %>%
      as.data.frame()
  })
  sched.l <- reactive({
    build_schedule(courses.df()[[1]],courses.df()[[2]])
  })
  cal.plots <-reactive({
    get_plots(sched.l(), dep.data)
  })
  output$courses.output <- renderText({
    #paste(courses.df()[[1]], courses.df()[[2]], collase = ", ", sep = " ")'
    as.character(courses.df())
  })
}

shinyApp(ui, server)


