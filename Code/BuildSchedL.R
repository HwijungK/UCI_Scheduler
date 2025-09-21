# BUILD A SCHEDULE

build_schedule <- function(deptcode, coursenum) {
  start.time <- Sys.time()
  dep.data <<- search_course(deptcode, coursenum) |>
    create_date_time()
  cat("Time to get data:", round(Sys.time() - start.time, 2), "\n")
  sched.l <- get_sched_combo(deptcode, coursenum, dep.data)
  #cat("Time: ", as.character(Sys.time() - start.time), "\n")
  #cat("Total Possible Schedules: ", length(sched.l), "\n")
  cat("Time Taken:", round(Sys.time() - start.time, 2), "\n")
  return (sched.l)
}
