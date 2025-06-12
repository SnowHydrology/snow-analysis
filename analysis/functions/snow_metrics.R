# Functions to compute snow metrics

# Keith Jennings 
# 2025-06-10

# Note: These are standalone functions and relatively 'fragile'
# TODO: Much more work can, and should, be done to harden them against missing data, date issues, etc.

# TODO: Finish filling out roxygen skeletons

# Functions include those that compute:
# Maximum SWE
# Maximum SWE timing
# Snow cover duration
# First snow day
# Snow off day
# Snowmelt season length
# Snowmelt rate during metl season
# April 1st SWE
# Pre maximum SWE snowmelt amount
# Pre maximum SWE snowmelt percent of total melt
# Pre maximum SWE snowmelt as proportion of peak SWE
# Center of mass of snowmelt
# Snow seasonality metric 
# SWE:P ratio
# TODO: Add SSI from Hale et al. (2023)

#' Compute maximum snow water equivalent when given a series of SWE values
#'
#' @param SWE SWE data
#'
#' @returns Maximum snow water equivalent
#' @export
#'
#' @examples
maxSWE <- function(SWE){
  max(SWE, na.rm = T)
}

#' Compute the water year day of maximum snow water equivalent when given a series of SWE and DOWY values
#'
#' @param SWE SWE data
#' @param DOWY Day of water year (Oct. 1 = 1, Sep. 30 = 365 or 366)
#'
#' @returns The DOWY when Max SWE occurs
#' @export
#'
#' @examples
maxSWE_DOWY <- function(SWE, DOWY){
  DOWY[which.max(SWE)]
}

#' Compute snow cover duration: the total number of days in a water year with snow cover
#'
#' @param SWE SWE data
#' @param SWE_THRESH The SWE threshold over which we determine snow cover to be present (default = 0)
#'
#' @returns Total number of days with snow cover
#' @export
#'
#' @examples
scd <- function(SWE, SWE_THRESH=0){
  sum(SWE > SWE_THRESH, na.rm = T)
}

#' Compute the date (day of water year) of first snow 
#'
#' @param SWE SWE data
#' @param DOWY Day of water year (Oct. 1 = 1, Sep. 30 = 365 or 366)
#' @param SWE_THRESH The SWE threshold over which we determine snow cover to be present (default = 0)
#'
#' @returns The day of water year (1-366) of the first measurable snow cover
#' @export
#'
#' @examples
firstSnow <- function(SWE, DOWY, SWE_THRESH=0){
  DOWY[min(which(SWE > SWE_THRESH))]
}

#' Compute the snow-off date (day of water year): the first day after max SWE with 0 snow 
#'
#' @param SWE SWE data
#' @param DOWY Day of water year (Oct. 1 = 1, Sep. 30 = 365 or 366)
#' @param MAX_SWE_DOWY Day of water year when max SWE occurs
#' @param SWE_THRESH The SWE threshold over which we determine snow cover to be present (default = 0)
#'
#' @returns The day of water year (1-366) when snow disappears after max SWE
#' @export
#'
#' @examples
lastSnow <- function(SWE, DOWY, MAX_SWE_DOWY, SWE_THRESH=0){
  DOWY[min(which(SWE <= SWE_THRESH & DOWY > MAX_SWE_DOWY))]
}

#' Compute the total melt season length in days 
#'
#' @param MAX_SWE_DOWY max SWE day of water year
#' @param SNOW_OFF_DOWY snow-off day of water year
#'
#' @returns Total number of days between max SWE and the snow off DOWY
#' @export
#'
#' @examples
meltSeason <- function(MAX_SWE_DOWY, SNOW_OFF_DOWY){
  SNOW_OFF_DOWY - MAX_SWE_DOWY
}

#' Compute the average melt season snowmelt rate
#'
#' @param MELT_SEASON Length of melt season (between max SWE DOWY and snow off DOWY) in days
#' @param MAX_SWE Maximum snow water equivalent
#'
#' @returns Average melt season snowmelt rate
#' @export
#'
#' @examples
meltRate <- function(MELT_SEASON, MAX_SWE){
  MAX_SWE / MELT_SEASON
}

#' Compute the SWE on April 1st, the traditional water supply forecast date
#'
#' @param SWE SWE data
#' @param DOWY Day of water year (Oct. 1 = 1, Sep. 30 = 365 or 366)
#' @param WYEAR Water year
#'
#' @returns SWE on April 1
#' @export
#'
#' @examples
april1SWE <- function(SWE, DOWY, WYEAR){
  ifelse(WYEAR[1] %% 4 == 0,
         SWE[min(which(DOWY == 184))],
         SWE[min(which(DOWY == 183))])
}

#' Compute total snowmelt that occurs before max SWE
#'
#' @param SWE Snow water equivalent
#' @param DOWY Day of water year
#' @param MAX_SWE_DOWY Max SWE day of water year
#'
#' @returns Total snowmelt before max SWE
#' @export 
#'
#' @examples
preMaxMelt <- function(SWE, DOWY, MAX_SWE_DOWY){
  tmp_swe <- SWE[DOWY < MAX_SWE_DOWY]
  tmp_melt <- tmp_swe - lag(tmp_swe)
  sum(tmp_melt[tmp_melt < 0], na.rm = T)
}

#' Compute pre-max melt as percent of total melt
#'
#' @param PRE_MAX_MELT Total snowmelt before max SWE
#' @param SWE Snow water equivalent
#'
#' @returns
#' @export
#'
#' @examples
preMaxMeltPctTotalMelt <- function(PRE_MAX_MELT, SWE){
  # Time series of change in SWE
  tmp_melt <- SWE - lag(SWE)
  # Sum negatives (i.e. melt)
  total_melt <- sum(tmp_melt[tmp_melt < 0], na.rm = T)
  # Compute percent of total 
  (PRE_MAX_MELT / total_melt) * 100
}


#' Pre-max SWE snowmelt to peak SWE ratio
#'
#' @param PRE_MAX_MELT Total snowmelt before max SWE
#' @param MAX_SWE Max SWE 
#'
#' @returns Total snowmelt before max SWE
#' @export 
#'
#' @examples
preMaxMeltMaxSWERatio <- function(PRE_MAX_MELT, MAX_SWE){
  (PRE_MAX_MELT * -1) / MAX_SWE
}

#' Compute the snowmelt center of mass day of water year
#' This assumes negative SWE changes are snowmelt and we compute melt across the 
#' whole water year, not just the "melt season"
#'
#' @param SWE Snow water equivalent
#' @param DOWY Day of water year
#'
#' @returns The day of the water year when the center of mass of snowmelt occurs
#' @export
#'
#' @examples
meltCoM <- function(SWE, DOWY){
  # Compute the change in SWE
  tmp_melt <- na.omit(SWE - lag(SWE))
  # Cumulative sum of snowmelt
  cumulative_melt <- cumsum(ifelse(tmp_melt < 0,
                                   tmp_melt,
                                   0))
  # Total annual snowmelt
  total_melt = sum(tmp_melt[tmp_melt < 0])
  # Snowmelt center of mass
  center_melt = total_melt / 2
  # Difference from center of snowmelt mass
  melt_diff_from_center = abs(cumulative_melt - center_melt)
  # Day of water year of difference (+1 to account for lag trim)
  which.min(melt_diff_from_center) + 1
}

#' Compute the snow seasonality metric (SSM) from Petersky and Harpold (2018)
#' Petersky, R., & Harpold, A. (2018). 
#'     Now you see it, now you don't: a case study of ephemeral snowpacks and soil 
#'     moisture response in the Great Basin, USA. 
#'     Hydrology and Earth System Sciences, 22(9), 4891-4906.
#'
#' @param SWE Snow water equivalent 
#' @param SWE_THRESH Threshold above which we consider snow cover to be present (default = 0)
#' @param DAY_THRESH The number of continuous days when we consider snow cover to be seasonal (default = 60)
#'
#' @returns The SSM from -1 to 1 where -1 is totally ephemeral and 1 is totally seasonal
#' @export
#'
#' @examples
snowSeasonality <- function(SWE, SWE_THRESH=0, DAY_THRESH=60){
  snow_flag = ifelse(SWE > SWE_THRESH, 1, 0)
  tmp_rle <- rle(snow_flag)
  tmp_summary <- data.frame(consecutive_days = tmp_rle$lengths, snow_flag = tmp_rle$values)
  seasonal_days = sum(tmp_summary$consecutive_days[tmp_summary$consecutive_days >= DAY_THRESH & 
                                                     tmp_summary$snow_flag == 1])
  ephemeral_days = sum(tmp_summary$consecutive_days[tmp_summary$consecutive_days < DAY_THRESH & 
                                                     tmp_summary$snow_flag == 1])
  ssm = (seasonal_days - ephemeral_days) / 
    (seasonal_days + ephemeral_days)
  ssm
}

#' Compute the ratio of maximum SWE to total annual precipitation (SWE:P ratio)
#'
#' @param MAX_SWE Maximum snow water equivalent
#' @param PPT Precipitation
#'
#' @returns SWE:P ratio
#' @export
#'
#' @examples
sweToPptRatio <- function(MAX_SWE, PPT){
  MAX_SWE / sum(PPT, na.rm = T)
}