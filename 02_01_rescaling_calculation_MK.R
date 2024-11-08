##############################
##
## Calculate the Rescaling Factor 
## to approximate the predicted value based on the reported FAO data 
## by MinAh Kim
## Oct 22, 2024
##
##############################


library(dplyr)
library(purrr)
library(readr)

calculate_rescaling_factor <- function(iso, crop, min_crop, season_list, water_list, irri_rn_limit,
                                       variability_increase = 1, 
                                       nitr = 40,
                                       maxylr = 20000,
                                       co2_constant =370,
                                       input_fd = "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/",
                                       fd = "/Volumes/T9/"){

  
  #######################
  ## 0. Set Variables
  #######################
  
  # ISO (Country Code)
  country_fullname <- case_when(
    iso == "GHA" ~ "Ghana",
    iso == "ETH" ~ "Ethiopia"
  )
  
  #crop
  crop_fullname <- case_when(
    crop == "grou" ~ "Groundnuts, with shell",
    crop == "maiz" ~ "Maize",
    crop == "teff" ~ "Cereals nes"
  )
  
  # Set Console
  flush.console()
  cat("Calculating Rescaling Factor")

  
  #######################
  ## 1. Read Data
  #######################
  
  # Read the FAO Data
  fao_data <- read.csv(paste0(input_fd, "mnyld2016to2020.csv"))
  # Read the yield that we need to calculate the rescaling factor 
  fao_yield <- fao_data %>% filter(area == country_fullname & item == crop_fullname) %>% pull(yld)
  
  # Read Historical & No Climate Change Data (Function)
  read_csv_for_season <- function (season){
    
    
    # Read from 1 to 1000
    expand_grid(data.frame(x = seq(1, 901, by = 100), y = seq(100, 1000, by = 100)), water_list) |>
      
      pmap( \(x, y, water_list) tryCatch({
        
        # Read csv
        read_csv(paste0(fd, iso, "/", iso, "_", water_list, "_", min_crop, "_hist&nocc_", season, "_pixel_", x, "to", y, ".csv"))
        
      }, warning = function(war){
        
        # When warning, regenerate the according file
        source("01_01_hfd_agera5_by_pixel_MK.R")
        
        # Regenerate the according file that returns a warning
        yield_generator(
          iso = iso,
          crop = crop, 
          min_crop = min_crop,
          season_list = season,
          scenario_list = c("hist&nocc"),
          water_list = water_list,
          irri_rn_limit = irri_rn_limit,
          sample_beg_list = x,
          sample_end_list = y,
          variability_increase = variability_increase,
          nitr = nitr,
          maxylr = maxylr,
          co2_constant =co2_constant,
          input_fd = input_fd,
          fd = fd
          )
      }
        )
        ) |>
      
      # Select columns of interest
      map(\(x) select(x, c(yr, prod, hect))) %>%
      
      # Bind rows into one dataframe
      bind_rows()
    }

  # Read all data on hist&nocc
  histnocc <- season_list |>
    map(read_csv_for_season) %>%
    bind_rows()
  
  #######################
  ## 2. Calculate 
  #######################
  
  # Calculate the total yield between the year 2016 and 2020, which the FAO data represents  
  total_yield <- histnocc %>%
    filter(yr %in% 2016:2020) %>%
    summarize(yld_summary = sum(prod)/sum(hect)) %>%
    pull(yld_summary)
  
  # Calculate the ratio
  rescaling_factor <- fao_yield/total_yield
  
  return(rescaling_factor)
}





