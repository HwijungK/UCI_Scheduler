library(googlesheets4)
library(googledrive)
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(sets)
library(stringr)
library(rvest)
library(utils)

# create vector of abrevations of departments by scraping from dep dropdown select in soc homepage
base.url <- "https://www.reg.uci.edu/perl/WebSoc"
dep.abrvs <- base.url %>%
  read_html() %>%
  html_nodes("select") %>%
  .[3] %>%
  html_children() %>%
  html_attr("value")%>% 
  .[-1]
dep.abrvs.encoded <- URLencode(dep.abrvs, reserved = T)

year.term <- "2025-92"

urls <- paste("https://www.reg.uci.edu/perl/WebSoc?YearTerm=", year.term, "&ShowFinals=1&ShowComments=0&Dept=", dep.abrvs.encoded, sep="", end="")

dep.raw.data <- NULL
for (i in 1:length(urls)) {
  print(dep.abrvs[i])
  url <- urls[i]
  tables <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
  if (!is.null(tables[2][[1]])) {
    dep.raw.data <- rbind(tables[[2]][1:18],dep.raw.data)
  }
  
}
 


dep.data <- dep.raw.data
dep.abrv <- dep.abrvs

# names columns of data frame
colnames(dep.data) <- unlist(strsplit("Code,Type,Sec,Units,Instructor,Modality,Time,Place,Final,Max,Enr,WL,Req,Nor,Rstr,Textbooks,Web,Status", ","))

header.rows <- NULL ## Logical Vector that Stores rows that are headers or null
course.col <- NULL ## new column that contains the name of the course in front of each entry
curr.course <- NULL

#############################################################################################################
#-------------------------------------------------------- Format Data
#############################################################################################################
#dep.data <- t

for (i in 1:nrow(dep.data)) {
  c <- unlist(dep.data[[i,1]])[1]
  if (!is.null((c))) {
    c <- as.character(c)
    if (any(startsWith(toupper(c), dep.abrv))) {
      curr.course <- c
    } else if (!is.null(curr.course)) {
      course.col[i] <- curr.course
    }
  }
  if (is.null(c) | c == "") {
    header.rows[i] <- T
  }
  else {
    header.rows[i] <- (c == "Code" | is.na(suppressWarnings((as.numeric(c))))| any(startsWith(toupper(c), dep.abrv)) | is.null(curr.course) | startsWith(c, "(same"))
  }
}

for(i in 1:ncol(dep.data)) {
  if (class(dep.data[1,i][[1]]) == "list") {
    print(i)
    dep.data[i] <- as.vector(dep.data[i])
  }
}

#Add Raw.Course column
dep.data <- cbind("Course.Raw" = course.col, dep.data)

# Remove All rows that are not Classes
dep.data <- dep.data[!header.rows,]


# removed original numbering of rows which are now messed up after deleting certain rows
row.names(dep.data) <- NULL

# create new columns for course names
course.identifier <- gsub("[ ]+", " ", trimws(substr(dep.data$Course.Raw, stop = str_locate(dep.data$Course.Raw, "[A-Z]*[0-9]+[A-Z]*")[,2] + 1, start = 1)))
course.remove.suffix <- gsub("\\(Prerequisites\\)", "", dep.data$Course.Raw)
course.name <- gsub("[ ]+", " ", (trimws(substr(course.remove.suffix, start = str_locate(course.remove.suffix, "[A-Z]*[0-9]+[A-Z]*")[,2] + 1, stop = nchar(dep.data$Course.Raw)), whitespace = "[\\h\\v\t\r\n ]")))
dep.data <- cbind(Course.Identifier=course.identifier, Course.Name=course.name, dep.data)
