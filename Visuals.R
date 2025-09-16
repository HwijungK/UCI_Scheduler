library(ggplot2)
library(tidyverse)

cal.plots <- list()

  for (i in 1:length(sched.l)) {
    print(i)
    courses.in.sched <- filter(dep.data, Code %in% sched.l[[i]])
    
    
    codes.v <- NULL
    for(code in courses.in.sched$Code) {
      class.count <- courses.in.sched[courses.in.sched$Code == code,]$intervals |>
        str_split("\\|") |>
        unlist() |>
        length() |>
        (\(x) x/2)()
      codes.v <- append(codes.v, rep(code, class.count))
    }
    
    i.start <- (int_start(char_to_int_v(courses.in.sched$intervals)))
    i.end <- (int_end(char_to_int_v(courses.in.sched$intervals)))
    day_of_week <- wday(i.start)
    start_times <- hour(i.start) + (minute(i.start) * 1/60)
    end_times <- hour(i.end) + (minute(i.end) * 1/60)
    
    cal <- data.frame(day_of_week, start_times, end_times, Code = codes.v)
    cal <- merge(cal, courses.in.sched, by = "Code")
    cal.plot<-ggplot(data = cal, aes(label = courseTitle)) +
      geom_rect(aes(xmin=day_of_week-.4, xmax = day_of_week+.4, ymin = start_times, ymax = end_times)) +
      geom_label(aes(x = day_of_week, y =start_times), nudge_y = -.15, label.padding = unit(.25,"lines"), label.size=.25) +
      scale_y_reverse(breaks = 7:21, limits = c(19,8)) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_line(linewidth = 3))
    
    cal.plots[[i]] <- cal.plot
  }

