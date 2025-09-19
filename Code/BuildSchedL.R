# BUILD A SCHEDULE


start.time <- Sys.time()
dep.data <- search_course(c("BIO SCI", "BIO SCI", "MATH"), c("2A", "93", "3A")) |>
  create_date_time()
sched.l <- get_sched_combo(c("BIO SCI", "BIO SCI", "MATH"), c("2A", "93", "3A"))
cat("Time: ", as.character(Sys.time() - start.time), "\n")

cat("Total Possible Schedules: ", length(sched.l))
cat("Time Taken:", round(Sys.time() - f.startTime, 2))
