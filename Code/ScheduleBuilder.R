#UCI Sched THIS IS THE RIGHT ONE!!!!!!!!!!!!!!!!!

library(googlesheets4)
library(googledrive)
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(sets)
library(stringr)

##############################################################################################################################################################################
#--------------------------------------------------------------------Finding classes via name
##############################################################################################################################################################################


find_classes <- function(name, dep.data) {
  return(filter(dep.data, str_equal(courseTitle, name, ignore_case = T)|str_equal(paste(deptCode, courseNumber, sep = " "), name, ignore_case = T)))
}

##############################################################################################################################################################################
#--------------------------------------------------------------------CREATING DATE TIMES
##############################################################################################################################################################################

####Summary: Add a new column to department data that consists of a string representation of the intervals of time taken up by said class

# given vector of intervals, return string in format "start_time"|"end_time"|"start_time"|"end_time"
int_v_to_char <- function(int_v) {
  r.char <- NULL
  for (intv.index in 1:length(int_v)) {
    r.char <- c(r.char, as.character(int_start(int_v[intv.index])), as.character(int_end(int_v[intv.index])))
  }
  return(paste(r.char, collapse = "|"))
}

# given character form of vector of intervals, return vector of intervals
char_to_int_v <- function(char) {
  int.v <- list()
  char.v <- unlist(base::strsplit(char, "\\|"))
  #return(char.v)
  
  for (i in 1:(length(char.v)/2)) {
    s.time <- ymd_hms(char.v[(i-1)*2 + 1])
    e.time <- ymd_hms(char.v[(i-1)*2 + 2])
    int.v <- append(int.v, lubridate::interval(s.time, e.time))
  }
  return(int.v)
} # Original

char_to_int_v <- function(char) {
  
  #debug
  #start.time <- Sys.time()
  #char <- dep.data[dep.data$Code == "38965",]$intervals
  
  char.v <- unlist(base::strsplit(char, "\\|"))
  #return(char.v)
  
  int.v <- lubridate::interval(start = char.v[seq(1, length(char.v), 2)], end = char.v[seq(2,length(char.v), 2)])
  
  #cat("Time: ", Sys.time() - start.time, "\n")
  #0.0206
  
  return(int.v)
}



create_date_time <- function(dep.data) {
  # splits time column into vector of days ending with the time period
  test <- dep.data |> 
    mutate(
      Time = gsub(" ", "", Time),
      dotw = gsub("([a-zA-Z])([0-9])", "\\1 \\2", Time),
      dotw = gsub("\U00A0", " ", dotw),
      dotw = gsub("([A-Z])", " \\1", dotw)
    )|>
    select("dotw")
  
  dotw.dict <- c("Su"="2025-9-7", "M"="2025-9-1", "Tu"="2025-9-2", "W"="2025-9-3", "Th"="2025-9-4", "F"="2025-9-5", "Sa"="2026-9-6")
  
  test <- test[[1]]
  # convert test vector into dates
  date.list <- list()
  intervals.col.l <- list()
  for(i in 1:length(test)) {
    #for(i in 7827) {
    #for(i in 22) {
    # if TBA, add NA entry
    if (!startsWith(test[i], " T B A") && !endsWith(test[i], "T B A") && test[i] != "") {
      v <- unlist(gsub("\n", " ",test[i]) |>
                    strsplit(" "))
      # remove empty "" entry
      v <- v[nzchar(v)]
      
      day.v <- NULL
      interval.list <- list()
      for (k in 1:length(v)) {
        if (v[k] %in% c("M", "Tu", "W", "Th", "F", "Sa", "Su")) {
          day.v <- append(day.v, v[k])
        }
        else {
          
          start.clock.time <- strsplit(v[k], "-")[[1]][1]
          end.clock.time <- strsplit(v[k], "-")[[1]][2]
          pm <- endsWith(end.clock.time, "p")
          for (j in 1:length(day.v)) {
            start.time <- ymd_hm(paste(dotw.dict[day.v[j]], start.clock.time))
            end.time <- ymd_hm(paste(dotw.dict[day.v[j]], end.clock.time))
            # Debug
            if (is.na(pm) | is.null(pm)) {
              print(i)
              print(v[k])
            }
            
            if (pm) {
              
              # Debug
              if (is.na(hour(end.time) | is.null(hour(end.time)))){
                print(test[i-2])
                print(test[i])
                print(i)
                #print(paste(test[i], "|", end.time, "|", strsplit(v[k], "-")[[1]][2], "|", dotw.dict[day.v[j]],"|", day.v))
              } 
              
              
              if (hour(end.time) < 8) {
                hour(end.time) <- 12 + hour(end.time)
              }
              if (hour(start.time) < 8) {
                hour(start.time) <- 12 + hour(start.time)
              }
            }
            interval.list <- append(interval.list, lubridate::interval(start.time, end.time))
          }
          day.v <- NULL
        }
      }
      #print(interval.list)
      intervals.col.l[[i]] = interval.list
    }
    else {
      intervals.col.l[[i]] = NA
    }
  }
  dep.data <- tibble::add_column(dep.data, intervals=intervals.col.l)
  return(dep.data)
}
############################################################################################################################
#-----------------------------------------------------------------------------CREATE CLASS SECTION COMBONATIONS
############################################################################################################################
get_class_combo <- function(name, dep.data) {
  # Return vector of codes that represent all possible combinations of sections you can take for a give class
  #name <- "I&C Sci   6D     *DISCRET MATH FOR CS*      (Prerequisites)"
  #name <- "I&C Sci   31     *INTRO TO PROGRMMING*      (Prerequisites)"
  #name <- "I&C Sci   139W     *CRITICAL WRITING*      (Prerequisites)"
  #name <- "I&C Sci   51     *INTRO COMPUTER ORG*      (Prerequisites)"

  
  # Creates subset with only classes in specific course
  classes.in.course <- find_classes(name, dep.data) |>
    mutate(
      # Creates Letter column which contains the Letter of Section (Ex: 3A --> A)
      Letter = gsub("([0-9])", "", Sec),
      across(where(is.character), ~ na_if(., ""))
      #Letter = gsub(" ", NA, Letter),
    ) |>
    select(c("courseTitle", "Type", "Sec", "Letter", "Code"))
  
  
  types <- unique(classes.in.course$Type)
  letters <- unique(classes.in.course$Letter)
  letters <- letters[!is.na(letters)]
  
  total.perms <- NULL
  perms <- NULL
  roots <- c("")
  for (letter.I in 1:length(letters)) {
    letter.filtered.classes <- filter(classes.in.course, Letter %in% c(NA, letters[letter.I]))
    for (type.I in 1:length(types)) {
      l.t.filtered.classes <- filter(letter.filtered.classes, Type == types[type.I])
      for (class.I in 1:nrow(l.t.filtered.classes)) {
        for (root.I in 1:length(roots)) {
          #cat(root.I, roots[root.I], collapse = ":", end = "\n")
          perms <- append(perms, paste(roots[root.I], l.t.filtered.classes$Code[class.I]))
          
        }
      }
      roots <- perms
      #print(perms)
      perms <- NULL
    }
    total.perms <- append(total.perms, roots)
    roots <- c("")
  }
  total.perms <- trimws(total.perms, which = "left")
  return(total.perms)
}

# DEMO
#get_class_combo("I&C Sci   6B     *BOOL LOG & DISC STR*      (Prerequisites)")


############################################################################################################################
#-----------------------------------------------------------------------------CREATE Schedule COMBONATIONS
############################################################################################################################
get_sched_combo <- function(depcodes, coursenums, dep.data) {
  # create a list where each entry is a course containing a vector of viable class combos
  class.combos.l <- list()
  for (name in paste(depcodes, coursenums, sep=" ")) {
    class.combos.l[[name]] = get_class_combo(name, dep.data)
  }
  #print("class.combos.l:\n")
  #print(class.combos.l)
  
  ####
  #cat("Time to create Class Combo List:", Sys.time() - debug.start.time)
  #debug.start.time <- Sys.time();
  
  # create sched.l a list  containing all possible courses that haven't been timechecked. each component of the list is a vector containing courses
  sched.v <- "";
  for (class.combos in class.combos.l) {
    #print(course)
    for (i in 1:length(sched.v)) {
      base <- sched.v[1]
      for (class.combo in class.combos) {
        sched.v <- append(sched.v, paste(base,class.combo))
      }
      sched.v <- sched.v[-1]
    }
  }
  sched.v <- trimws(sched.v)
  #print(class.combos.l)
  sched.l <- strsplit(sched.v, split = " ")
  ####
  #cat("Time to create sched.l:", Sys.time() - debug.start.time)
  #debug.start.time <- Sys.time();
  
  debug.ogsched <<- sched.l
  # create a dataframe where row name is each class and one column contains string representations of all classes that conflict with said class
  filter.start.time <- Sys.time()
  sched.status.open.check.l <- filter_status(dep.data, sched.l, c("OPEN", "Waitl"))
  sched.l <- sched.l[sched.status.open.check.l]
  cat("Time to filter status:", (Sys.time() - filter.start.time), "\n")
  debug.status.sched.l <<- sched.l
  
  filter.start.time <- Sys.time()
  sched.time.check.l<-filter_time_conflict(dep.data, sched.l)
  sched.l <- sched.l[sched.time.check.l]
  cat("Time to filter time conflicts:", (Sys.time() - filter.start.time), "\n")
  debug.sched.l <<- sched.l
  
  return(sched.l)
}
#takes in a list of possible schedules and returns a logical vector for classes that are not full
filter_status <- function(dep.data, sched.l, status) {
  lapply(sched.l, \(x) {
    statuses <- filter(dep.data, Code %in% x)$Status
    #print(statuses)
    #print(filter(dep.data, Code %in% x))
    if (all(statuses %in% status)) {
      return(T)
    }
    else {
      return(F)
    }
  }) |>
    unlist()
}

# takes in a list of possible schedules and returns a logical vector
filter_time_conflict <- function(dep.data, sched.l) {
  # create a vector of course codes present in each schedule
  class.v <- unique(unlist(sched.l))
  class.compatable.df <- data.frame(compatable=rep("", length(class.v)))
  row.names(class.compatable.df) <- class.v
  
  for(c1 in 1: (length(class.v) - 1)) {
    for (c2 in (c1+1):length(class.v)) {
      compatable = T
      intervals <- c((filter(dep.data, Code == class.v[c1])$intervals[[1]]),
                     (filter(dep.data, Code == class.v[c2])$intervals[[1]]))
      int_combn <- combn(as.list(intervals), 2,simplify = T)
      if (any(is.na(intervals))) {
        compatable <- FALSE
      }
      else {
        compatable <- apply(int_combn, 2, \(x) {
        lubridate::int_overlaps(x[[1]], x[[2]])
        }) |>
          (\(x) !x)() |>
          all()
      }
      
      ## DEBUG
      if (is.na(compatable)) {
        print(intervals)
        print(!any(is.na(intervals)))
      }
      
      if (compatable) {
        class.compatable.df[c1,1] <- paste(class.compatable.df[c1,1], trimws(class.v[c2]))
        class.compatable.df[c2,1] <- paste(class.compatable.df[c2,1], trimws(class.v[c1]))
      }
    }
}
  
  # removes schedules that have overlapping classes on sched.l
  sched.time.check.l <- rep(T, length(sched.l))
  for (i in length(sched.l):1) {
    sched <- sched.l[[i]]
    for (c1 in 1:(length(sched)-1)) {
      for (c2 in (c1 + 1):length(sched)) {
        if (!grepl(sched[c2], class.compatable.df[as.character(sched[c1]), 1])) {
          sched.time.check.l[i] <- F
          #print(strsplit(class.compatable.df[as.character(sched[c1]),1], " "))
          #cat("incompatable: ", sched[c1],",",sched[c2],"\n")
        }
        if (!sched.time.check.l[i]) break;
      }
      if (!sched.time.check.l[i]) break;
    }
  }
  return (sched.time.check.l)
}


#### TESTING
# start.time <- Sys.time()
# dep.data <- search_course(c("I&C SCI", "I&C SCI", "WRITING", "MATH"), c("32", "6B", "60", "3A")) |>
#   create_date_time()
# sched.l <- get_sched_combo(c("I&C SCI", "I&C SCI", "WRITING", "MATH"), c("32", "6B", "60", "3A"))
# cat("Time: ", as.character(Sys.time() - start.time), "\n")

#using bio sci 93, i&c sci 32, I&C sci 6b
#16 # original
#12 # improved char_to_interval 
#4 "idk lmao



