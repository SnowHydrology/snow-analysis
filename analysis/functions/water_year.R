# Functions to calculate the water year and day of water year

# Keith Jennings 
# 2025-06-10

# Install lubridate for date functions
library(lubridate)

#' Compute the water year (Oct. 1 through Sep. 30)
#'
#' @param date Formatted date object
#'
#' @returns Water year 
#' @export
#'
#' @examples wateryear(ymd("2024-04-15"))
wateryear <- function(date){
  ifelse(month(date) >= 10,
         year(date) + 1,
         year(date))
}

#' Compute the day of the water year (Oct. 1 = 1, Sep. 30 = 365 or 366)
#'
#' @param date Formatted date object
#'
#' @returns Day of water year (1 to 366)
#' @export
#'
#' @examples day_of_wateryear(ymd("2024-04-15"))
day_of_wateryear <- function(date){
  ifelse(year(date) %% 4 != 0,
         ifelse(month(date) >= 10,
                yday(date) - 273,
                yday(date) + 92),
         ifelse(month(date) >= 10,
                yday(date) - 274,
                yday(date) + 92))
}