##############################
##
## Calculate the Yield with Season
## by MinAh Kim
## Sept 19, 2024
##
##############################


yield_generator <- function(
    iso,
    crop, 
    min_crop,
    season_list,
    scenario_list,
    water_list,
    irri_rn_limit,
    sample_beg_list = seq(1, 901, by = 100),
    sample_end_list = seq(100, 1000, by = 100),
    variability_increase = 1,
    hist_nocc_variability = FALSE,
    nitr = 40,
    maxylr = 20000,
    co2_constant =370,
    input_fd = "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/",
    fd = "/Volumes/T9/"
  ){
  
  ########################
  ## Function that Generates the Yield based on country
  ## Parameters: 
  ##    iso : string. three letter country code
  ##.   crop : string. four letter name of the crop
  ##    min_crop: string. two letter name of the crop
  ##
  ##    season_list : list(). Include the seasons that need to be applied in yield generation
  ##    scenario_list : list(). Include Greenhouse gas emission scenarios that need to be applied in yield generation
  ##    water_list : list(). Include water source ("rnfd" or "irri" = rainfed or irrigation) that need to be applied in yield generation
  ##
  ##    variability_increase: numeric. The amount of variability that is added for precipitation.
  ##                          If we want the precipitation variability to incresase 14%, we set this parameter as '1.14'. 
  ##                          If 1, no variability is added.
  ##    hist_nocc_variability: boolean. If TRUE, the increase in precipitation variability applies for hist&nocc. If FALSE, the variability_increase value is not applied to hist&nocc
  ##
  ##    farming_month : numeric. Determine the number of month from the beginning of planting to be counted in the model. Default as 4.
  ##    nitr : numeric. Nitrogen level
  ##    maxylr: numeric. Maximum Yield to record after applying the emulator.
  ##    co2_constant: numeric. CO2 Level set to apply to the emulator
  ##
  ##    input_fd: directory with all input file
  ##    fd: directory with all output file
  ########################  
  
  ### 0. Set Variables & Packages
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  
  # Code with all the functions
  source("02_03_01_hfd_agera5_by_pixel_function_MK.R")
  
  # 01. Calculate the country pixel
  # Confirmed to have same result as 02_01
  country_pixel <- read_country_pixel(iso = iso, input_fd = input_fd)
  
  # 02. Function 
  run_scen_season_water <- function(iso, scen, season, water, crop, min_crop, variability_increase, hist_nocc_variability, irri_rn_limit, nitr, maxylr, co2_constant, country_pixel){
    
    # Farming Month is calculated by the length of the vector that contains the minimum irrigation rainfall for each month
    farming_month <- length(irri_rn_limit) 
    
    # Year (Beginning and Ending Year of the Sample)
    begyr <- ifelse(scen == "hist&nocc", 2000, 2020)
    endyr <- 2069 
    
    print("===================================")
    print(paste(Sys.time(), "Begin a new set of variables :", scen, season, water, sep = " "))
    print(paste0(Sys.time(), " Read Planting Calendar"))
    
    # 02-01. Read Planting Calendar
    planting_calendar <- read_planting_calendar(iso, crop, min_crop, season, water, country_pixel = country_pixel, 
                                                input_fd = input_fd)
    
    # 02-02. Combine Country Pixel and Planting Calendar
    # Confirmed to have same result as 02_01
    country_calendar <- inner_join(country_pixel, planting_calendar, by = c("x2", "y2"))
    
    # 02-03. Calculate the yield For Each Sample 
    list(sample_beg_list, sample_end_list) |> 
      pwalk(\(x, y) climate_to_yield_by_sample(scen = scen, 
                                               season = season,
                                               water = water, 
                                               min_crop = min_crop,
                                               begyr = begyr, 
                                               endyr = endyr, 
                                               country_calendar = country_calendar, 
                                               sample_beg = x, 
                                               sample_end = y,
                                               variability_increase = variability_increase,
                                               hist_nocc_variability = hist_nocc_variability,
                                               irri_rn_limit = irri_rn_limit,
                                               farming_month = farming_month,
                                               nitr = nitr,
                                               maxylr = maxylr, 
                                               co2_constant = co2_constant,
                                               iso = iso,
                                               input_fd = input_fd,
                                               fd = fd
                                               ))
    
  }
  
  # 03. Apply the function
  data.frame(expand_grid(scenario_list, season_list, water_list))  %>%
    rename(scen = scenario_list, season = season_list, water = water_list) %>%
    pwalk(\(scen, season, water) run_scen_season_water(iso = iso,
                                                       scen = scen, 
                                                       season = season, 
                                                       water = water, 
                                                       crop = crop, 
                                                       min_crop = min_crop, 
                                                       variability_increase = variability_increase,
                                                       hist_nocc_variability = hist_nocc_variability,
                                                       irri_rn_limit = irri_rn_limit,
                                                       nitr = nitr,
                                                       maxylr = maxylr,
                                                       co2_constant = co2_constant,
                                                       country_pixel = country_pixel
                                                       ))
  
}


