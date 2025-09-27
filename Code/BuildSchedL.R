# BUILD A SCHEDULE
get_depdata <- function(deptcode, coursenum) {
  if (any(deptcode == "")) {return(NULL)}
  dep.data <- search_course(deptcode, coursenum) |>
    create_date_time()
  debug.dep.data <<- dep.data
  return(dep.data)
}
build_schedule <- function(deptcode, coursenum) {
  if (any(deptcode == "")) {return(NULL)}
  debug.d <<- deptcode
  debug.c <<- coursenum
  start.time <- Sys.time()
  dep.data <- get_depdata(deptcode, coursenum)
  debug.dep.data <<- dep.data
  cat("Time to get data:", round(Sys.time() - start.time, 2), "\n")
  sched.l <- get_sched_combo(deptcode, coursenum, dep.data)
  #cat("Time: ", as.character(Sys.time() - start.time), "\n")
  #cat("Total Possible Schedules: ", length(sched.l), "\n")
  cat("Time Taken:", round(Sys.time() - start.time, 2), "\n")
  return (sched.l)
}
