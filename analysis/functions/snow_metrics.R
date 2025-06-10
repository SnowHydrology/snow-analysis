# Functions to compute snow metrics

# Keith Jennings 
# 2025-06-10

# Note: These are standalone functions and relatively 'fragile'
# Much more work can, and should, be done to harden them against
# missing data, date issues, etc.

# Functions include those that compute:
# Peak SWE
# Peak SWE timing
# Snow cover duration
# First snow day
# Snow off day
# Snow melt season
# Snowmelt rate
# Pre peak melt
# SWE:P ratio
# Center of mass of snowmelt
# TODO: Kate's SSI

#' Compute maximum snow water equivalent when given a series of SWE values
#'
#' @param SWE 
#'
#' @returns Maximum snow water equivalent
#' @export
#'
#' @examples
maxSWE <- function(SWE){
  max(SWE, na.rm = T)
}

maxSWE_DOWY <- function(SWE, DOWY){
  DOWY[which.max(SWE)]
}

scd <- function(SWE, SWE_THRESH=0){
  sum(SWE > SWE_THRESH, na.rm = T)
}

firstSnow <- function(SWE, DOWY, SWE_THRESH=0){
  DOWY[min(which(SWE > SWE_THRESH))]
}

lastSnow <- function(SWE, DOWY, MAX_SWE_DOWY, SWE_THRESH=0){
  DOWY[min(which(SWE <= SWE_THRESH & DOWY > MAX_SWE_DOWY))]
}

meltSeason <- function(MAX_SWE_DOWY, SNOW_OFF_DOWY){
  SNOW_OFF_DOWY - MAX_SWE_DOWY
}

meltRate <- function(MELT_SEASON, MAX_SWE){
  MAX_SWE / MELT_SEASON
}

april1SWE <- function(SWE, DOWY, WYEAR){
  ifelse(WYEAR[1] %% 4 == 0,
         SWE[min(which(DOWY == 184))],
         SWE[min(which(DOWY == 183))])
}

preMaxMelt <- function(SWE, DOWY, MAX_SWE_DOWY){
  tmp_swe <- SWE[DOWY < MAX_SWE_DOWY]
  tmp_melt <- tmp_swe - lag(tmp_swe)
  sum(tmp_melt[tmp_melt < 0], na.rm = T)
}

meltCoM <- function(SWE, DOWY){
  tmp_melt <- na.omit(SWE - lag(SWE))
  cumulative_melt <- cumsum(ifelse(tmp_melt < 0,
                                   tmp_melt,
                                   0))
  total_melt = sum(tmp_melt[tmp_melt < 0])
  center_melt = total_melt / 2
  melt_diff_from_center = abs(cumulative_melt - center_melt)
  which.min(melt_diff_from_center) + 1
}
tmp <- df %>% filter(wyear == 2012) %>%  select(date, snow_water_equivalent, dowy)
tmp$swe_change <- tmp$snow_water_equivalent - lag(tmp$snow_water_equivalent)
tmp <- tmp %>% na.omit()

tmp$cumulative_melt <- cumsum(ifelse(tmp$swe_change < 0,
                                     tmp$swe_change,
                                     0)) 

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

sweToPptRatio <- function(MAX_SWE, PPT){
  MAX_SWE / sum(PPT, na.rm = T)
}
  
  
  
funtest <- df %>% 
  group_by(wyear) %>% 
  summarize(max_swe = maxSWE(snow_water_equivalent), 
            max_swe_dowy = maxSWE_DOWY(snow_water_equivalent, dowy), 
            scd_days = scd(snow_water_equivalent),
            snow_on_day = firstSnow(snow_water_equivalent, dowy),
            snow_off_day = lastSnow(snow_water_equivalent, dowy, max_swe_dowy),
            melt_season_days = meltSeason(max_swe_dowy, snow_off_day),
            melt_rate = meltRate(melt_season_days, max_swe),
            ssm = snowSeasonality(snow_water_equivalent), 
            swe_apr1 = april1SWE(snow_water_equivalent, dowy, wyear), 
            pre_max_swe_melt = preMaxMelt(snow_water_equivalent, dowy, max_swe_dowy),
            melt_com = meltCoM(snow_water_equivalent, dowy),
            swe_to_ppt = sweToPptRatio(max_swe, precipitation))

cumsum(c(1, 2, 3, NA, 5))
