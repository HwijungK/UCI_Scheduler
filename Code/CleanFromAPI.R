
library(httr2)
library(jsonlite)
library(tidyverse)

base_url <- "https://anteaterapi.com/v2/rest/websoc"

soc_response <- request(base_url) |>
  req_url_query(
    "year"="2025",
    "quarter"="Fall"
  ) |>
  req_perform() |>
  resp_body_json()

soc_response |>
  pluck('data', 'schools', 8, 'departments', 2, 'courses', 3, 'sections', 1) |>
  glimpse()

# # returns glimpse of school
# soc_response |>
#   pluck('data', 'schools', 9, 'departments', 2, 'courses', 5, 'sections', 1, 'meetings') |>
#   View()
# 
# # returns list of school names
# soc_response |> 
#   pluck('data', 'schools',) |>
#     map_dfr(
#       function(x) {
#         tibble(
#           schoolName = x |> pluck("schoolName")
#         )
#       }
#     )

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
