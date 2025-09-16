# BUILD A SCHEDULE



f.startTime <- Sys.time()
sched.l <- get_sched_combo(c("bio sci 93", "I&C Sci 32", "Stats 67"))
for (i in 1:length(sched.l)) {
  print(paste(sched.l[[i]], collapse = ", "))
}
cat("Total Possible Schedules: ", length(sched.l))
cat("Time Taken:", round(Sys.time() - f.startTime, 2))

