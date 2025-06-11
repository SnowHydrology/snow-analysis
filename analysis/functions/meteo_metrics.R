# Functions to compute meteorological data metrics

# Keith Jennings 
# 2025-06-11

# Note: These are standalone functions and relatively 'fragile'
# TODO: Much more work can, and should, be done to harden them against missing data, date issues, etc.

# TODO: Finish filling out roxygen skeletons

# Functions include those that compute:
# Total annual precip
# Seasonal precip
# Mean annual temperature
# Seasonal temperature

totalPrecip <- function(PRECIP, na.rm=TRUE){
  if(na.rm == TRUE) PRECIP <- na.omit(PRECIP)
  sum(PRECIP)
}

seasonalPrecip <- function(PRECIP, DATE, SEASON, na.rm=TRUE){
  valid_ssns = c("spring", "summer", "fall", "winter")
  if(!(SEASON %in% valid_ssns)){
    cat("Invalid season, must be one of:", valid_ssns, "\n")
    return()
  } else{
    if(na.rm == TRUE) PRECIP <- na.omit(PRECIP)
    ifelse(SEASON == "spring",
           sum(PRECIP[month(DATE) %in% 4:6]),
           ifelse(SEASON == "summer",
                  sum(PRECIP[month(DATE) %in% 7:9]),
                  ifelse(SEASON == "fall",
                         sum(PRECIP[month(DATE) %in% 10:12]),
                         sum(PRECIP[month(DATE) %in% 1:3]))))
  }
}

meanTemp <- function(TEMP, na.rm=TRUE){
  if(na.rm == TRUE) TEMP <- na.omit(TEMP)
  mean(TEMP)
}

seasonalTemp <- function(TEMP, DATE, SEASON, na.rm=TRUE){
  valid_ssns = c("spring", "summer", "fall", "winter")
  if(!(SEASON %in% valid_ssns)){
    cat("Invalid season, must be one of:", valid_ssns, "\n")
    return()
  } else{
    if(na.rm == TRUE) TEMP <- na.omit(TEMP)
    ifelse(SEASON == "spring",
           mean(TEMP[month(DATE) %in% 4:6]),
           ifelse(SEASON == "summer",
                  mean(TEMP[month(DATE) %in% 7:9]),
                  ifelse(SEASON == "fall",
                         mean(TEMP[month(DATE) %in% 10:12]),
                         mean(TEMP[month(DATE) %in% 1:3]))))
  }
}
