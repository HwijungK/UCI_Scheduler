# BUILD A SCHEDULE

build_schedule <- function(deptcode, coursenum) {
  start.time <- Sys.time()
  dep.data <<- search_course(deptcode, coursenum) |>
    create_date_time()
  sched.l <- get_sched_combo(deptcode, coursenum, dep.data)
  cat("Time: ", as.character(Sys.time() - start.time), "\n")
  
  cat("Total Possible Schedules: ", length(sched.l))
  cat("Time Taken:", round(Sys.time() - start.time, 2))
  return (sched.l)
}
