##############################
##
## Calculate the Yield with Season (Function Collection)
## by MinAh Kim
## Sept 19, 2024
##
##############################


### 0. Set Variables & Packages

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(purrr)
library(haven)
library(lubridate)
library(readr)

### 1. Country Pixel

read_country_pixel <- function(iso, input_fd){
  
  # ===========================================================
  ### Import the pixel list and subset for a specific country ###
  # ===========================================================
  
  ## Parameter 
  # iso : string. Three letter iso code for a country
  # input_fd : string. The location of the agera5_iso_gadm2p7_global_v2.csv file. It should end with a "/"
  
  ## Return
  # a dataframe with the column (xlon, ylat, iso3, xbig, ybig, ktpix, x2, y2)
  # xlon, ylat  : geographical coordinate
  # x2, y2      : the result after multiplying xlon and ylat to 10 and dropping all decimal place. These are column created to ease future dataframe joins. 
  # iso3        : iso code of a country
  # xbig, ybig  : the coordinate used to join with the future hfd data

  # Import the agera5_iso_gadm2p7_global_v2.csv file for country level pixels
  country_pixel <- read.csv(paste(input_fd, "agera5_iso_gadm2p7_global_v2.csv", sep="")) %>%
    filter(iso3 == iso) %>%
    mutate(
      x2 = round(10*xlon, digits=0),
      y2 = round(10*ylat, digits=0)
    )
  
}


### 2. Prepare Planting Calendar & Pixel

read_planting_calendar <- function(iso, crop, min_crop, season, water, country_pixel,
                                   input_fd = "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/"){
  
  # Import the Ethiopia Maize Planting Calendar    
  if(iso == "ETH" & crop == "maiz"){
    
    # Code for reading the dta file (new calendar)
    planting_calendar <- read_dta(paste0(input_fd, "agera5_", min_crop, "_planting_months_by_pixel_0p2_v3.dta")) %>%
      
      # Rename the variables
      rename(
        x2 = x1,
        y2 = y1,
        xlon = x,
        ylat = y
      ) %>%
      
      # Remove crop name for better generability. Remove the crop name from all variable names that start with hect_
      rename_with(~str_replace(.x ,paste0(crop, "_"),""), starts_with("hect_"))%>%
      
      mutate(
        
        # Fill out xlon and ylat for missing values 
        # -- but may not be necessary since it will be merged with country pixel by x2
        xlon = ifelse(is.na(xlon), as.numeric(x2)/10, xlon),
        ylat = ifelse(is.na(ylat), as.numeric(y2)/10, ylat), 
        
        # Recording season by darpa
        annual_season = case_when(
          is.na(mo_belg_darpa) & is.na(mo_meher_darpa) ~ "Meher",
          is.na(mo_belg_darpa) & !is.na(mo_meher_darpa) ~ "Meher",
          !is.na(mo_belg_darpa) & is.na(mo_meher_darpa) ~ "Belg",
          !is.na(mo_belg_darpa) & !is.na(mo_meher_darpa) ~ "Meher and Belg",
        )
      ) %>%
      
      # Split the dataframe by season to convert to long
      group_by(annual_season) %>%
      group_split() %>%
      map(
        ~{
          # Write seas as an individual season and annual season the record that this is a location with both Meher and Belg 
          if(unique(.x$annual_season) == "Meher and Belg"){
            # Create one row for Meher and another for Belg
            bind_rows(.x %>% mutate(seas = "Meher"),
                      .x %>% mutate(seas = "Belg"))
            
          } else {
            # For season that only have Meher or Belg, record just Meher or Belg for annual_season
            .x %>% mutate(seas = annual_season)
          }
        }
      ) %>%
      bind_rows() %>%
      mutate(
        # Set the beginning of farming month based on the rules
        mo = case_when(
          (annual_season == "Meher and Belg") & (seas == "Meher") ~ 8,
          seas == "Belg" ~ 3,
          annual_season == "Meher" ~ mo_ggcmi, 
          TRUE ~ NA
        ),
        # Divide the hectare for places with both Meher and Belg
        hect_rnfd = ifelse(annual_season == "Meher and Belg", hect_r/2, hect_r),
        hect_irri = ifelse(annual_season == "Meher and Belg", hect_i/2, hect_i)
      ) %>%
      select(c("x2", "y2", "seas", "mo", paste0("hect_", water))) %>%
      rename(any_of(c("hect" = "hect_irri", "hect" = "hect_rnfd"))) %>%
      filter(seas == str_to_title(season))
    
  # Import the Ethiopia Teff Planting Calendar    
  } else if(iso == "ETH" & crop == "teff"){
    
    # Code for reading the csv file
    planting_calendar <- read_csv(paste0(input_fd, "agera5_tf_planting_months_by_pixel_0p2_v3.csv")) %>%
      rename(x2 = x1, y2 = y1) %>%
      mutate(xlon = x2/10, ylat = y2/10,
             annual_season = case_when(both_types == 1 ~ "both_type",
                                       meher_only == 1 ~ "meher_only",
                                       belg_only == 1 ~ "belg_only"
                                       )) %>%
      group_by(annual_season) %>%
      group_split() %>%
      map(~{
        if(unique(.x$annual_season) == "both_type"){
          
          bind_rows(
            .x %>% mutate(mo = mo_meher_in_belg, 
                          seas = "Meher",
                          hect_rnfd = rfhect_meher,
                          hect_irri = irhect_meher),
            .x %>% mutate(mo = mo_belg, 
                          seas = "Belg",
                          hect_rnfd = rfhect_belg,
                          hect_irri = irhect_belg)
          )
          
        } else if (unique(.x$annual_season) == "meher_only"){
          .x %>% mutate(mo = mo_meher_only,
                        seas = "Meher",
                        hect_rnfd = rfhect_meher,
                        hect_irri = irhect_meher)
          
        } else if (unique(.x$annual_season) == "belg_only"){
          .x %>% mutate(mo = mo_belg,
                        seas = "Belg",
                        hect_rnfd = rfhect_belg,
                        hect_irri = irhect_belg)
        }
      }) %>%
      bind_rows() %>%
      select(c("x2", "y2", "seas", "mo", paste0("hect_", water))) %>%
      rename(any_of(c("hect" = "hect_irri", "hect" = "hect_rnfd"))) %>%
      filter(seas == str_to_title(season))
    
    
  } else{
    
    min_water <- ifelse(water == "rnfd", 'r', 'i')
    
    # Add Hectare from SPAM
    spam <- read.csv(paste(input_fd, "spam2017_", crop,"_v2.csv", sep="")) %>%
      rename(hect = paste("hect", crop, min_water, sep ="_")) %>%
      select(c("xlon", "ylat", "hect"))
      
    # Add SPAM to country pixel
    country_hect <- left_join(country_pixel, spam, by=c("xlon", "ylat"))
      
    min_water <- ifelse(water == "rnfd", 'rf', 'ir')
    
    # Import planting date calendar from (GGCMI)
    ggcmi <- read.csv(paste(input_fd, "agera5_ggcmi_pltgdt_", min_water, min_crop, ".csv", sep=""))
    
    # Add Planting Calendar
    planting_calendar <- left_join(country_hect, ggcmi, by = c("xlon", "ylat")) %>%
      rename(pltg = ends_with("ggcmi")) %>%
      mutate(pltg = month(ymd("2021-01-01") + days(pltg) - days(1)))
    
  }
  
  return(planting_calendar)
}

### 3. Read Climate and Calculate the Yield
climate_to_yield_by_sample <- function(scen, season, water, min_crop, begyr, endyr, country_calendar, sample_beg, sample_end, 
                                       variability_increase, irri_rn_limit, farming_month, nitr, maxylr, co2_constant, iso,
                                       save_csv = TRUE,
                                       hist_nocc_variability = FALSE,
                                       input_fd = "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/",
                                       fd = "/Volumes/T9/"){
  
  print(paste(Sys.time(), "Climate to Yield - Processing Sample", sample_beg, "to", sample_end, sep = " "))
  
  ### 3-1 Read Climate Data
  
  # Keep variability 1 for hist&nocc
  variability_increase <- ifelse((scen == "hist&nocc") & (hist_nocc_variability == FALSE), 1, variability_increase)
  
  ## ==== Temperature =====
  # Read Temperature
  t <- readRDS(paste0(fd, iso, "/" ,iso, "_sample_tmax_", scen, "_", sample_beg, "to", sample_end, ".rds"))
  
  # Filter the temperature between the beginning year and the ending year
  # (Written in BaseR instead of dplyr to improve performance)
  t <- t[t$yr >= begyr & t$yr <= endyr,]

  ## ==== Precipitation ====
  # If variability is added to precipitation
  if(variability_increase != 1){
    
    # Read the file that has the right degree of variability in the name
    p <- readRDS(paste0(fd, iso, "/" , iso, "_sample_prec_", "var_", variability_increase, "_", scen, "_", sample_beg, "to", sample_end, ".rds"))
    
  } else{
    
    # When no variability is included, read the precipitation file that does not has the precipitation in the title
    p <- readRDS(paste0(fd, iso, "/" ,iso, "_sample_prec_", scen, "_", sample_beg, "to", sample_end, ".rds"))
  }
  
  # Filter the precipitation between the beginning year and the ending year
  # (Written in BaseR instead of dplyr to improve performance)
  p <- p[p$yr >= begyr & p$yr <= endyr,]  
  
  # From the beginning of the sample to the end of the sample
  c(sample_beg:sample_end) |>
    
    # Apply piecewise function for each sample
    map(\(x) piecewise_function(t = t, p = p, 
                                water = water, 
                                min_crop = min_crop,
                                begyr = begyr, endyr = endyr, 
                                country_calendar = country_calendar, 
                                chosen_menu_row = x, 
                                farming_month = farming_month, 
                                nitr = nitr, maxylr = maxylr, 
                                co2_constant = co2_constant, 
                                input_fd = input_fd,
                                irri_rn_limit = irri_rn_limit)) %>%
    # Bind the result of all sample 
    bind_rows() %>%
    
    # Save result in csv (Different name depends on variability)
    
    {if((variability_increase !=  1) & save_csv) {
    
      write.csv(x = ., file = paste0(fd, iso, "/", paste(iso, water, min_crop, "var", variability_increase, scen, season, "pixel", paste0(sample_beg, "to", sample_end), sep = "_"), ".csv")) 
  
     } else if((variability_increase == 1) & save_csv){
        
      write.csv(x = ., file = paste0(fd, iso, "/", paste(iso, water, min_crop, scen, season, "pixel", paste0(sample_beg, "to", sample_end), sep = "_"), ".csv"))
     
      } else .
    }
    
}

### 4. Transform Data to Fit the Piecewise Function For Each Sample
piecewise_function <- function(t, p, water, min_crop, begyr, endyr, country_calendar, 
                               chosen_menu_row, farming_month, nitr, maxylr, co2_constant, input_fd,
                               irri_rn_limit){
  
  ## ================
  ## Prepare the Data
  ## ================
  
  # Read in the tmax and precipitation data generated from climate_to_yield_by_sample 
  ti <- t %>% 
    
        # Filter by a single sample
        filter(menu_row == chosen_menu_row) %>% 
        
        # Select the needed columns
        select(c("xlon", "ylat", "yr", "tx1", "tx2", "tx3", "tx4", "tx5", "tx6", "tx7", "tx8", "tx9", "tx10","tx11","tx12"))
  
  pi <-  p %>%
        
        # Filter by a single sample
        filter(menu_row == chosen_menu_row) %>%
        
        # Select the needed columns
        select(c("xlon", "ylat", "yr", "pr1", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7", "pr8", "pr9", "pr10", "pr11", "pr12")) 
  
  # Join t and p
  ci <- full_join(ti, pi, by=c("xlon", "ylat", "yr")) %>%
    
        # Generate x2 and y2 to merge with the country_calendar
        mutate(
          x2 = round(10*xlon, digits = 0),
          y2 = round(10*ylat, digits = 0)
        ) %>%
    
        # Remove xlon and ylat
        select(-c("xlon", "ylat"))
      
  
  # Join with country_calendar by pixel
  # inner join results same as 02_01
  # Farming Month filtering results the same as 02_01
  ci <- inner_join(country_calendar, ci, by= c("x2", "y2")) %>%
    
    # Pivot Long so each row will have the value for a single month
    pivot_longer(
      cols = starts_with(c("tx", "pr")),
      cols_vary = "slowest",
      names_to = c("variable", "month"),
      names_pattern = "([a-z]{2})(\\d+)"
    ) %>%
    
    # Ensure that month is numeric so the calculation doesn't cause error in the next step
    mutate(month = as.numeric(month)) %>%
      
    ## ================
    ## Create tx and rn within Farming Months
    ## ================
    
    filter(ifelse(mo + farming_month -1 > 12, 
                  (month >= mo) | (month < mo + farming_month-12) , 
                  (month >= mo) & (month < mo + farming_month))) %>%
    
    # Change month to relative month (e.g. 1 for first month of farming)
    # Change pr to rn
    # Create the variable_month that shows both variable and month
    mutate(month = ifelse(month >= mo, month - mo + 1 , (month+12) - mo + 1),
           variable = ifelse(variable == "pr", "rn", variable),
           variable_month = paste0(variable, month)) %>%
    
    # Set minimum rainfall by month for irrigation based on the values of irri_rn_limit
    mutate(value = ifelse(((water == "irri") & (variable == "rn")) & (value < irri_rn_limit[month]), irri_rn_limit[month], value)) 
    
  
  ## ================
  ## Create gt variables for tx and rn to apply the piecewise function
  ## ================  
  ci_tx <- ci[ci$variable == "tx", ] %>%
    
    # Separate the dataframe by month
    group_by(month) %>%
    group_split() |>
    map(~{
      
      # Record the variable (should be tx)
      unique_var <- unique(.x$variable)  
      # Record the month (should be a single value that represents the relative month of the points in the sliced dataframe)
      unique_month <- unique(.x$month)
      
      .x %>%
        mutate(
          # Create variables for the piecewise function (based on intervals)
          gt23 = ifelse(value >= 23, value - 23, 0),
          gt25 = ifelse(value >= 25, value - 25, 0),
          gt27 = ifelse(value >= 27, value - 27, 0),
          gt29 = ifelse(value >= 29, value - 29, 0),
          gt31 = ifelse(value >= 31, value - 31, 0),
          gt34 = ifelse(value >= 34, value - 34, 0)
        ) %>%
        
        # Format the name to be "variable month_gt number" (e.g. tx1_gt23)
        rename_with(
          ~ paste0(unique_var, unique_month, "_", .x),
          starts_with("gt")
        ) %>%
        
        # Exclude columns not needed
        select(-c("variable_month", "variable", "month", "value")) %>%
        
        # Change to long format
        pivot_longer(
          cols = starts_with("tx"),
          names_to = "variable",
          values_to = "value"
        )
        
      }) %>%
    bind_rows()
  
  
  ci_rn <- ci[ci$variable == "rn", ] %>%
    group_by(month) %>%
    group_split() |>
    map(~{
      
      # Record the variable (should be rn)
      unique_var <- unique(.x$variable)  
      # Record the month (should be a single value that represents the relative month of the points in the sliced dataframe)
      unique_month <- unique(.x$month)
      
      .x %>%
        
        mutate(
          # Create variables for the piecewise function (based on intervals)
          gt25 = ifelse(value >= 25, value - 25, 0),
          gt50 = ifelse(value >= 50, value - 50, 0),
          gt100 = ifelse(value >= 100, value - 100, 0),
          gt200 = ifelse(value >= 200, value - 200, 0),
          gt300 = ifelse(value >= 300, value - 300, 0),
        ) %>%
        
        # Format the name to be "variable month_gt number" (e.g. rn1_gt100)
        rename_with(
          ~ paste0(unique_var, unique_month, "_", .x),
          starts_with("gt")
        ) %>%
        
        # Exclude columns not needed
        select(-c("variable_month", "variable", "month", "value")) %>%
        
        # Change to long format
        pivot_longer(
          cols = starts_with("rn"),
          names_to = "variable",
          values_to = "value"
        )
      
      }) %>%
    bind_rows()
    
  # Combine the variables created for the piecewise function and the original variables
  # ci_tx, ci_rn, and the ci (which has changed names to bind with ci_tx and ci_rn)
  # Result identical to 02_01
  ww <- rbind(ci_tx, ci_rn, ci %>% select(-c ("month", "variable")) %>% rename(variable = variable_month))
  
  ### 5. Apply the Piecewise Function (Function Below)
  apply_the_piecewise_function(ww, chosen_menu_row, min_crop, begyr, endyr, nitr, maxylr, co2_constant, input_fd)
  
}

### 5. Apply the Piecewise Function 
apply_the_piecewise_function <- function(ww, chosen_menu_row, min_crop, begyr, endyr, nitr, maxylr, co2_constant, input_fd){
  
  ## ================
  ## Prepare Variables
  ## ================   
  # Read the parameter for the Piecewise Function
  parameter_for_piecewise <- read.csv(paste0(input_fd, min_crop, "_emulator_parameters_for_r_v2.csv")) 
  
  # Read a row that we will use as a parameter
  # Currently hard coded as the 4th row
  # Need to Create a code to decide with row
  parameter_for_piecewise <- parameter_for_piecewise[4,]

  # Widen the variables created using prepare_piecewise_function
  emulator_variables <- ww %>% 
    pivot_wider(names_from = variable, values_from = value) %>%
    
    # Include the CO2 Value. Currently coded for a case when CO2 value is a constant
    mutate(co2 = co2_constant)
  
  ## ================
  ## Apply Piecewise Function
  ## ================   
  emulator_variables$yld <- exp(
    
      # CO2
      parameter_for_piecewise$co2a * emulator_variables$co2 +
      # Squared CO2
      parameter_for_piecewise$co2a_sq * ((emulator_variables$co2)^2) +

      # Rainfall
      parameter_for_piecewise$rn1 * emulator_variables$rn1 +
      parameter_for_piecewise$rn1_gt25 * emulator_variables$rn1_gt25 +
      parameter_for_piecewise$rn1_gt50 * emulator_variables$rn1_gt50 +
      parameter_for_piecewise$rn1_gt100 * emulator_variables$rn1_gt100 +
      parameter_for_piecewise$rn1_gt200 * emulator_variables$rn1_gt200 +
      parameter_for_piecewise$rn1_gt300 * emulator_variables$rn1_gt300 +
        
      parameter_for_piecewise$rn2 * emulator_variables$rn2 +
      parameter_for_piecewise$rn2_gt25 * emulator_variables$rn2_gt25 +
      parameter_for_piecewise$rn2_gt50 * emulator_variables$rn2_gt50 +
      parameter_for_piecewise$rn2_gt100 * emulator_variables$rn2_gt100 +
      parameter_for_piecewise$rn2_gt200 * emulator_variables$rn2_gt200 +
      parameter_for_piecewise$rn2_gt300 * emulator_variables$rn2_gt300 +  
      
      parameter_for_piecewise$rn3 * emulator_variables$rn3 +
      parameter_for_piecewise$rn3_gt25 * emulator_variables$rn3_gt25 +
      parameter_for_piecewise$rn3_gt50 * emulator_variables$rn3_gt50 +
      parameter_for_piecewise$rn3_gt100 * emulator_variables$rn3_gt100 +
      parameter_for_piecewise$rn3_gt200 * emulator_variables$rn3_gt200 +
      parameter_for_piecewise$rn3_gt300 * emulator_variables$rn3_gt300 + 
        
      parameter_for_piecewise$rn4 * emulator_variables$rn4 +
      parameter_for_piecewise$rn4_gt25 * emulator_variables$rn4_gt25 +
      parameter_for_piecewise$rn4_gt50 * emulator_variables$rn4_gt50 +
      parameter_for_piecewise$rn4_gt100 * emulator_variables$rn4_gt100 +
      parameter_for_piecewise$rn4_gt200 * emulator_variables$rn4_gt200 +
      parameter_for_piecewise$rn4_gt300 * emulator_variables$rn4_gt300 +  
         
      
      # Temperature
      parameter_for_piecewise$tx1 * emulator_variables$tx1 +
      parameter_for_piecewise$tx1_gt23 * emulator_variables$tx1_gt23 +
      parameter_for_piecewise$tx1_gt25 * emulator_variables$tx1_gt25 +
      parameter_for_piecewise$tx1_gt27 * emulator_variables$tx1_gt27 +
      parameter_for_piecewise$tx1_gt29 * emulator_variables$tx1_gt29 +
      parameter_for_piecewise$tx1_gt31 * emulator_variables$tx1_gt31 +
      parameter_for_piecewise$tx1_gt34 * emulator_variables$tx1_gt34 +
        
        
      parameter_for_piecewise$tx2 * emulator_variables$tx2 +
      parameter_for_piecewise$tx2_gt23 * emulator_variables$tx2_gt23 +
      parameter_for_piecewise$tx2_gt25 * emulator_variables$tx2_gt25 +
      parameter_for_piecewise$tx2_gt27 * emulator_variables$tx2_gt27 +
      parameter_for_piecewise$tx2_gt29 * emulator_variables$tx2_gt29 +
      parameter_for_piecewise$tx2_gt31 * emulator_variables$tx2_gt31 +
      parameter_for_piecewise$tx2_gt34 * emulator_variables$tx2_gt34 +
        
      
      parameter_for_piecewise$tx3 * emulator_variables$tx3 +
      parameter_for_piecewise$tx3_gt23 * emulator_variables$tx3_gt23 +
      parameter_for_piecewise$tx3_gt25 * emulator_variables$tx3_gt25 +
      parameter_for_piecewise$tx3_gt27 * emulator_variables$tx3_gt27 +
      parameter_for_piecewise$tx3_gt29 * emulator_variables$tx3_gt29 +
      parameter_for_piecewise$tx3_gt31 * emulator_variables$tx3_gt31 + 
      parameter_for_piecewise$tx3_gt34 * emulator_variables$tx3_gt34 +
        
        
      parameter_for_piecewise$tx4 * emulator_variables$tx4 +
      parameter_for_piecewise$tx4_gt23 * emulator_variables$tx4_gt23 +
      parameter_for_piecewise$tx4_gt25 * emulator_variables$tx4_gt25 +
      parameter_for_piecewise$tx4_gt27 * emulator_variables$tx4_gt27 +
      parameter_for_piecewise$tx4_gt29 * emulator_variables$tx4_gt29 +
      parameter_for_piecewise$tx4_gt31 * emulator_variables$tx4_gt31 +  
      parameter_for_piecewise$tx4_gt34 * emulator_variables$tx4_gt34 +
      
      parameter_for_piecewise$X_cons  
  )
  
  # Clean the result to return a dataframe in a desired format
  emulator_variables <- emulator_variables %>%
    filter(yr >= begyr & yr <= endyr) %>%
    mutate(
      overmax = ifelse(yld > maxylr, 1, 0),
      yld = ifelse(overmax == 1, maxylr, yld),
      prod = hect * yld,
      nitr = nitr,
      co2 = co2_constant,
      menu_row = chosen_menu_row
    ) %>%
    select(c("xlon", "ylat", "yr", "nitr", "co2", "hect", "yld", "overmax", "prod", "menu_row"))
}



