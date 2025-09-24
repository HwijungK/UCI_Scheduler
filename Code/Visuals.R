library(ggplot2)
library(tidyverse)

get_plots <- function(sched.l, dep.data) {
  b.start.time <- Sys.time()
  cal.plots <- list()
  
  for (i in 1:length(sched.l)) {
    #print(i)
    courses.in.sched <- filter(dep.data, Code %in% sched.l[[i]])
    
    
    codes.v <- NULL
    for(code in courses.in.sched$Code) {
      class.count <- courses.in.sched[courses.in.sched$Code == code,]$intervals[[1]] |>
        length()
      codes.v <- append(codes.v, rep(code, class.count))
    }
    #print(courses.in.sched)
    i.start <- lubridate::as_datetime(unlist(lapply(courses.in.sched$intervals, int_start)))
    i.end <- lubridate::as_datetime(unlist(lapply(courses.in.sched$intervals, int_end)))
    day_of_week <- wday(i.start)
    start_times <- hour(i.start) + (minute(i.start) * 1/60)
    end_times <- hour(i.end) + (minute(i.end) * 1/60)
    
    cal <- data.frame(day_of_week, start_times, end_times, Code = codes.v)
    cal <- merge(cal, courses.in.sched, by = "Code")
    
    cal.plot <- ggplot(data = cal, aes(label = courseTitle)) +
      geom_rect(aes(xmin=day_of_week-.45, xmax = day_of_week+.45, ymin = start_times, ymax = end_times)) +
      geom_label(aes(x = day_of_week, y =start_times), nudge_y = -.15, label.padding = unit(.25,"lines"), label.size=.25) +
      scale_y_reverse(breaks = 7:20, limits = c(20,7), labels = c(7:12, 1:8)) +
      #scale_y_reverse(limits = c(24,7)) +
      scale_x_continuous( limits = c(1.5,6.5),breaks = 2:6, labels =  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) +
      labs(x = "Days", y = "Time", title = paste("Codes:", paste(courses.in.sched$Code, collapse = ","))) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_line(linewidth = 3))
    
    cal.plots[[i]] <- cal.plot
  }
  cat("Build plot time: ", (Sys.time() - b.start.time), '\n')
  debug.plots <<- cal.plots
  return (cal.plots)
}

