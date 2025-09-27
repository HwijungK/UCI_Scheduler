
library(httr2)
library(jsonlite)
library(tidyverse)

# return the response from ant eater api given the string for year and quarter (Fall, Spring, Summer, Winter)
get_soc_response <- function(year, quarter) {
  base_url <- "https://anteaterapi.com/v2/rest/websoc"
  
  soc_response <- NULL
  soc_response <- request(base_url) |>
    req_url_query(
      "year"=year,
      "quarter"=quarter
    ) |>
    req_perform() |>
    resp_body_json()
  
  return (soc_response)
}

# returns a data frame containing all courses of a quarter
get_whole_catalog <- function() {
    d <- soc_response |>
    pluck('data', 'schools') |>
    map_dfr(
      \(schools.list) {
        tibble(
          schools.list |>
            pluck("departments") |>
              map_dfr(
                \(dep.list) {
                  tibble(
                      dep.list |> pluck ('courses') |>
                        map_dfr(
                        \(course.list) {
                          tibble (
                            course.list |> pluck('sections') |>
                              map_dfr(
                                \(sec.list) {
                                  tibble(
                                      #Code,Type,Sec,Units,Instructor,Modality,Time,Place,Final,Max,Enr,WL,Req,Nor,Rstr,Textbooks,Web,Status
                                      
                                      #school = schools.list |> pluck("schoolName"),
                                      #deptName = dep.list |> pluck('deptName'),
                                      Code = sec.list |> pluck('sectionCode'),
                                      Type = sec.list |> pluck('sectionType'),
                                      Sec = sec.list |> pluck ('sectionNum'),
                                      Units =  sec.list |> pluck('units'),
                                      Instructor = sec.list |> pluck('instructors') |> unlist() |> paste(collapse = "; "),
                                      Modality  = ":)",
                                      Time = sec.list |> pluck('meetings') |>
                                        lapply(\(x) {x[c('days', 'startTime','endTime')]}) |> unlist() |> paste(c(" ", ":", "-", ":", " "), recycle0 = T, collapse = "", sep = "") |>
                                        (\(s) {gsub(":0-", ":00-", s)})(),
                                      Place = ":)",
                                      Final = sec.list |> pluck('finalExam') |> unlist() |> paste(collapse = ", "),
                                      Max = sec.list |> pluck('maxCapacity'),
                                      Enr = sec.list |> pluck('numCurrentlyEnrolled', 1),
                                      WL = sec.list |> pluck('numOnWaitlist'),
                                      Req = sec.list |> pluck('numRequested'),
                                      Nor = sec.list |> pluck('numNewOnlyReserved'),
                                      Rstr = sec.list |> pluck('restrictions'),
                                      Textbooks = ":)",
                                      Status = sec.list |> pluck('status'),
                                      
                                      deptCode = course.list |> pluck('deptCode'),
                                      courseTitle = course.list |> pluck('courseTitle'),
                                      courseNumber = course.list |> pluck('courseNumber')
    
                                  )
                                }
                              )
                          )
                        }
                      )
                  )
                }
              )
        )
      }
    )
    return(d)
}

# returns a data frame consisting of sections in specified courses. takes in depcodes and courenums
search_course <- function(depcodes, coursenums) {
  # capitalizes course codes
  depcodes <- str_to_upper(depcodes)
  coursenums <- str_to_upper(coursenums)
  cat('searching for course code', paste(depcodes, coursenums, sep = " ", collapse = ", "))
  # #schools <- c("Donald Bren School of Information and Computer Sciences","School of Physical Sciences")
  # depcodes <- c("I&C SCI", "MATH")
  # coursenums <- c("31", "3A")
  # #depcodes <- "I&C SCI"
  
  schools <- NULL
  for (i in 1:17) {
    schools[which(depcodes %in% dept.school$deptCode[[i]])] <- dept.school$school[i]
  }
  
  sched.data <- soc_response |> pluck('data','schools') |>
    map_dfr(
      \(school.list) {
        if (school.list$schoolName %in% schools) {
          school.list |>
            pluck('departments') |>
            map_dfr(
              \(dep.list) {
                if (dep.list$deptCode %in% depcodes) {
                  dep.list |>
                    pluck('courses') |>
                    map_dfr(
                      \(course.list) {
                        if (course.list$courseNumber %in% coursenums) {
                          #course.list$courseTitle
                          course.list |> pluck('sections') |>
                            map_dfr(
                              \(sec.list) {
                                t <- tibble(
                                  #Code,Type,Sec,Units,Instructor,Modality,Time,Place,Final,Max,Enr,WL,Req,Nor,Rstr,Textbooks,Web,Status
                                  
                                  #school = schools.list |> pluck("schoolName"),
                                  #deptName = dep.list |> pluck('deptName'),
                                  Code = sec.list |> pluck('sectionCode'),
                                  Type = sec.list |> pluck('sectionType'),
                                  Sec = sec.list |> pluck ('sectionNum'),
                                  Units =  sec.list |> pluck('units'),
                                  Instructor = sec.list |> pluck('instructors') |> unlist() |> paste(collapse = "; "),
                                  Modality  = ":)",
                                  Time = sec.list |> pluck('meetings') |>
                                    lapply(\(x) {x[c('days', 'startTime','endTime')]}) |> unlist() |> paste(c(" ", ":", "-", ":", " "), recycle0 = T, collapse = "", sep = "") |>
                                    (\(s) {gsub(":0-", ":00-", s)})(),
                                  Place = ":)",
                                  Final = sec.list |> pluck('finalExam') |> unlist() |> paste(collapse = ", "),
                                  Max = sec.list |> pluck('maxCapacity'),
                                  Enr = sec.list |> pluck('numCurrentlyEnrolled', 1),
                                  WL = sec.list |> pluck('numOnWaitlist'),
                                  Req = sec.list |> pluck('numRequested'),
                                  Nor = sec.list |> pluck('numNewOnlyReserved'),
                                  Rstr = sec.list |> pluck('restrictions'),
                                  Textbooks = ":)",
                                  Status = sec.list |> pluck('status'),
                                  
                                  deptCode = course.list |> pluck('deptCode'),
                                  courseTitle = course.list |> pluck('courseTitle'),
                                  courseNumber = course.list |> pluck('courseNumber')
                                ) 
                              }
                            )
                        }
                      }
                    )
                }
              }
            )
        }
      }
    )
  return(sched.data)
}


########################################################################################################################################################


soc_response <- get_soc_response('2025', 'Fall')

dept.school <- soc_response |>
  pluck('data', 'schools') |>
  map_dfr(
    \(schools.list) {
      tibble(
        schools.list |>
          pluck("departments") |>
          map_dfr(
            \(dep.list) {
              tibble(
                school = schools.list |> pluck('schoolName'),
                deptCode = dep.list |> pluck('deptCode'),
              )
            }
          )
      )
    }
  )
dept.school <- aggregate(list(deptCode = dept.school$deptCode), by = list(school = dept.school$school), FUN = paste)




