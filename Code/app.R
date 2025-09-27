library(shinydashboard)
library(bslib)
library(magrittr)
library(here)

#source(here::here("Code", "BuildSchedL.R"))
#source(here::here("Code", "CleanFromAPI.R"))
#source(here::here("Code", "ScheduleBuilder.R"))
#source(here::here("Code", "Visuals.R"))
source("BuildSchedL.R")
source("CleanFromAPI.R")
source("ScheduleBuilder.R")
source("Visuals.R")

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
      height =1200,
      plotOutput("cal.plot", height = 800),
      textOutput("debug")
    )
  )
)

i <- reactiveVal(5)
server <- function(input, output, session) {
  # sched list selection
  observeEvent(input$inc.plot.index, {
    old_val <- i()
    i(min(old_val + 1, length(sched.l())))
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
                max = length(sched.l()),
                value = i(),
                step = 1,
                animate = F,
                width = '80%')
  })
  output$cal.plot <- renderPlot({
    get_plots(sched.l()[i()], get_depdata(courses.df()[[1]], courses.df()[[2]]))
  })
  output$debug <- renderText({
    output$cal.plot()
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
    #print(courses.df()[[1]])
    build_schedule(courses.df()[[1]],courses.df()[[2]])
  })
  output$courses.output <- renderText({
    #paste(courses.df()[[1]], courses.df()[[2]], collase = ", ", sep = " ")'
    as.character(courses.df())
  })
}

shinyApp(ui, server)


