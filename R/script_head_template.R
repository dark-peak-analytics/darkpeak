#' Function to make a Cost-effectiveness Plane plot from matrices of total costs and QALYs
#' @param date Date the day the file is created, in any format (default is System Date).
#' @param who A character string containing the names of authors.
#' @return a script header template to be used for projects.
#' @examples
#' script_head_template(date = Sys.Date(),who = "Robert A Smith & Paul P Schneider")

script_head_template <- function(date = Sys.Date(),
                          who = "Robert A Smith & Paul P Schneider"){

  writeLines(

    c(" ---------------------------",
      "Script name:",
      "Purpose of script:",
      paste("Author(s):",who),
      paste("Date Created:",date),
      paste("Copyright (c) Dark Peak Analytics", format(date, "%Y")),
      "Email: darkpeakanalytics@gmail.com",
       "---------------------------",
      "Notes:                      ",
       "---------------------------"))
}


