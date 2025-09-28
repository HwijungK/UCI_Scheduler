library(shinydashboard)
library(bslib)
library(magrittr)
library(here)

source("BuildSchedL.R")
source("CleanFromAPI.R")
source("ScheduleBuilder.R")
source("Visuals.R")

ui <- page_fillable(
  skin = 'green',
  # dashboardHeader(
  #   title = "UCI Schedule Crafter"
  # ),
  # dashboardSidebar(
  #   
  # ),
  layout_sidebar(
    sidebar = sidebar(
      width = '30%',
      card(
        card_header(
          "Search Courses"
        ),
        card_body(
          status = "info",
          # Course Selection
          width = NULL,
          textInput("courses.text", tags$p("enter courses seperated by commas (ex. \"bio sci 93, math 3a, i&c sci 32\")", style = "font-size:10px;")),
          checkboxInput("show.full", tags$a("Include Full Classes", style = "font-size: 12px"), F),
          checkboxInput("show.wait", tags$a("Include Waitlist Classes", style = "font-size: 12px"), F),
          input_task_button("courses.submit", "GO"),
          span(textOutput("courses.output"), style = "font-size: 10px")
        )
      ),
      card(
        card_header(
          "Select Index"
        ),
        card_body(
          padding = 2,
          gap = 1,
          uiOutput("slider"),
          layout_columns(
            actionButton("dec.plot.index", "Prev", style = "font-size: 12px"),
            actionButton("inc.plot.index", "Next", style = "font-size: 12px")
          )
          
        )
      )
    ), 
    card(
      width = 10,
      height =1000,
      plotOutput("cal.plot", height = 800),
      textOutput("debug")
    )
  )
)

i <- reactiveVal(1)
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
    if (is.null(sched.l())) return (NULL)
    sliderInput("plot.index.slider",
                label = "",
                min = 1,
                max = length(sched.l()),
                value = i(),
                step = 1,
                animate = F,
                width = '100%')
  })
  output$cal.plot <- renderPlot({
    p <- get_plots(sched.l()[i()], get_depdata(courses.df()[[1]], courses.df()[[2]]))
    if (is.null(p)) return()
    else {p}
  })
  output$debug <- renderText({
    if (is.null(sched.l())) return ("")
    paste(sched.l()[[i()]], collapse = ", ")
    })
  
  # course selection
  courses.df <- eventReactive(input$courses.submit, ignoreNULL = FALSE, {
    if (input$courses.text == "") return(NULL)
    c.list <- input$courses.text %>%
      strsplit(",[ ]*") %>%
      unlist() %>%
      sub("^(.*) (.*)$", replacement = "\\1;\\2", .) %>%
      strsplit(';')
    do.call(args = c.list, what = rbind) %>%
      as.data.frame()
  })
  sched.l <- reactive({
    if (is.null(courses.df())) return (NULL)
    #print(courses.df()[[1]])
    build_schedule(courses.df()[[1]],courses.df()[[2]], class.status())
  })
  output$courses.output <- renderText({
    if (is.null(sched.l())) {
      return (NULL)
    }
    #paste(courses.df()[[1]], courses.df()[[2]], collase = ", ", sep = " ")'
    t <- paste("Created Schedule For: ",
          paste(str_to_upper(courses.df()[[1]]), str_to_upper(courses.df()[[2]]), collapse = ", ", sep = " "))
    t
  })
  class.status <- reactive({
    c("OPEN", "Waitl", "FULL")[c(T, input$show.wait, input$show.full)]
  })
}

shinyApp(ui, server)


