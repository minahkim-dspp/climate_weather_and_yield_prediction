###########
#
#     Data Visualization
#     September, 12, 2024 
#     
#
###########

library(dplyr)

###########
### 1. Function for Reading the Files
###########
read_crop_file <- function(fd, iso, crop, scen, variability_increase, hist_nocc_variability, aggregation = "national", season = ""){
  
  ## =========================
  ## This function reads the crop file
  ## =========================
  # Parameter:
  #   fd (str)                      : file directory for the crop file
  #   iso (str)                     : three letter code that represents a country
  #   scen (str)                    : climate scenario based on GHG emission level
  #   season(str)                   : the name of season  
  #   variability_increase (dbl)    : the amount that was multipled to the precipitation variability
  #   hist_nocc_variability (bool)  : If false, the precipitation variability will not reflect the variability_increase value. FALSE is the default.
  #   aggregation (str)             : a string representing the aggregation level of the file. Currently, the option is "national", "regional", and "aez5". The default is "national".

  # Return:
  #   df (dataframe): the dataframe that represents the aggregated crop file 
  
    # If hist_nocc_variability is false, the variability_increase value does not apply to hist&nocc
    variability_increase_case <- ifelse(((scen == "hist&nocc") &(hist_nocc_variability == FALSE)), 1, variability_increase)
  
    # Set the name of the file based on the variability and the season
    file_name <- case_when(
      (season == "") & (variability_increase_case == 1) ~ paste(iso, "rescaled", crop, scen, aggregation, sep = "_"), 
      (season != "") & (variability_increase_case == 1) ~ paste(iso, "rescaled", crop, scen, season, aggregation, sep = "_"),
      (season == "") & (variability_increase_case != 1) ~ paste(iso, "rescaled", crop, "var", variability_increase_case, scen, aggregation, sep = "_"), 
      (season != "") & (variability_increase_case != 1) ~ paste(iso, "rescaled", crop, "var", variability_increase_case, scen, season, aggregation, sep = "_")
    )
    
    # Read the CSV file
    df <- read.csv(paste0(fd, iso, "/", file_name, ".csv")) %>% select(-"X")
    
    # Return the dataframe
    return(df)
}

read_climate_file <- function(fd, iso, scen, variability_increase, tmax_or_prec = "tmax" , hist_nocc_variability = hist_nocc_variability){
  
  ## =========================
  ## This function reads the climate (maximum daily temperature, precipitation) file
  ## =========================
  
  # Parameter:
  #   fd (str)                      : file directory for the crop file
  #   iso (str)                     : three letter code that represents a country
  #   scen (str)                    : climate scenario based on GHG emission level
  #   variability_increase (dbl)    : the amount that was multiplied to the precipitation variability
  #   tmax_or_prec (str)            : a string representing the variable to read. Current choices are "tmax" for maximum temperature and "prec" for precipitation. The default is "tmax".
  #   hist_nocc_variability (bool)  : If false, the precipitation variability will not reflect the variability_increase value. FALSE is the default.
  
  
  # Return:
  #   dataframe with the climate data satisfying the condition
  
  # For (1, 100) to (901, 1000)
  list(beg = seq(1, 901, by = 100), end = seq(100, 1000, by = 100)) %>%
    
    pmap(\(beg, end) {
      
      # If hist_nocc_variability is false, the variability_increase value does not apply to hist&nocc
      variability_increase_case <- ifelse(((scen == "hist&nocc") &(hist_nocc_variability == FALSE)), 1, variability_increase)
      
      # Provide different file name based on the variable & the degree of variability increase reflected
      file_name <- ifelse(((tmax_or_prec == "prec") & (variability_increase_case != 1)) , 
                          paste0(iso, "_sample_", tmax_or_prec, "_var_", variability_increase_case, "_", scen, "_", beg, "to", end, ".rds"),
                          paste0(iso, "_sample_", tmax_or_prec, "_",  scen, "_", beg, "to", end, ".rds"))   
      
      # Read the RDS file
      readRDS(paste0(fd, iso, "/", file_name))
    }) %>%
    
    # Bind rows to create a single dataframe
    bind_rows()
    
}


###########
### 2. Function to help join
###########
weighted_by_planting_season <- function(df, tmax_or_prec, plant_month_range = 4){
  
  ## =========================
  ## This function calculates the weighted annual values of the climate based on the planting season 
  ## =========================
  
  ## Parameter:
  #   df (dataframe)        : the dataframe with the climate value and the planting calendar
  #   tmax_or_prec (str)    : a string representing the climate value to calculate. Current choices are "tmax" for maximum temperature and "prec" for precipitation.
  #   plant_month_range(int): the number of months that the planting season lasts. The default is 4.
  
  ## Return:
  #   dataframe that has the weighted average of climate values for a location, year, and sample
  
  # If the df denotes the beginning of the planting season as mo, change to pltgmo
  if(sum(names(df) %in% "mo") > 0){
    df <- df %>% rename(pltgmo = mo)
  }
  
    df %>% 
      
      # Pivot to a long so each row represents the climate value of a specific month
      pivot_longer(
        cols = starts_with(ifelse(tmax_or_prec == "tmax", "tx", "pr")),
        names_to = "month",
        values_to = "value"
      ) %>% 
      
      mutate(
        
        # Change the variable class from string to numeric for month
        month = as.numeric(str_sub(month, start = 3)),
        
        # Calculate the end of the planting month based on the 
        pltgmo_end = ifelse(pltgmo+plant_month_range > 13, pltgmo+plant_month_range-12, pltgmo+plant_month_range)
      ) %>%
      
      # Filter the month that only falls between the pltgmo (starting of the planting season) and pltgmo_end (the end of the planting season)
      filter(ifelse(pltgmo < pltgmo_end,
                    (month >= pltgmo) & (month < pltgmo_end), 
                    (month >= pltgmo) | (month < pltgmo_end))) %>%
      
      # Remove pltgmo and pltgmo_end
      select(-c("pltgmo", "pltgmo_end")) %>%
      
      # Group the dataframe by geographical location, year, and the sample value
      group_by(xlon, ylat, yr, menu_row) %>%
      
      # Average all values that have the same geographical location, year, and the sample value
      mutate(weighted_value = mean(value))%>%
      
      ungroup() %>%
      
      # Remove month and value
      select(-c("month", "value")) %>%
      
      # Prevent duplication
      distinct() %>%
      
      # Rename the variables that represented the weighted annual average as txyr or pryr 
      rename_with(~ case_when(
        (.x == "weighted_value") & (tmax_or_prec == "tmax") ~ "txyr", 
        (.x == "weighted_value") & (tmax_or_prec == "prec") ~ "pryr",
        TRUE ~ .x))

}

weighted_by_hectare <- function(df, tmax_or_prec = "tmax"){
  
  ## =========================
  ## This function calculates the weighted annual values of the climate based on the hectares used for farming 
  ## =========================
  
  ## Parameter:
  #   df (dataframe)        : the dataframe with the (weighted) annual average of climate
  #   tmax_or_prec (str)    : a string representing the climate value to calculate. Current choices are "tmax" for maximum temperature and "prec" for precipitation.

  ## Return:
  #   dataframe that has the weighted average of an aggregation level by year and sample
  
  
  df %>%
    
    # Rename the variable of interest as "value" 
    rename_with(~ ifelse((.x == "txyr")|(.x == "pryr"), "value", .x)) %>%
    
    # Calculate the weighted average by hectare
    mutate(hect_value = hect * value) %>%
    group_by(yr, menu_row, seas) %>%
    summarize(weighted_value = sum(hect_value)/sum(hect)) %>%
    
    # Rename the column as weighted_txyr or weighted_pryr
    rename_with(~ case_when(
      (.x == "weighted_value") & (tmax_or_prec == "tmax") ~ "weighted_txyr", 
      (.x == "weighted_value") & (tmax_or_prec == "prec") ~ "weighted_pryr",
      TRUE ~ .x)) 
  
}

weighted_climate_for_scen_season_yr <- function(iso, scen, season, tmax_or_prec, variability_increase, crop, min_crop, chosen_yr, hist_nocc_variability, input_fd){
  
  ## =========================
  ## This function calculates the weighted annual values of the climate based on the planting season and hectare
  ## =========================
  
  # Parameter:
  #   fd (str)                      : file directory for the crop file
  #   iso (str)                     : three letter code that represents a country
  #   scen (str)                    : climate scenario based on GHG emission level
  #   season(str)                   : the name of season  
  #   tmax_or_prec (str)            : a string representing the climate value to calculate. Current choices are "tmax" for maximum temperature and "prec" for precipitation.
  #   variability_increase (dbl)    : the amount that was multipled to the precipitation variability
  #   crop (str)                    : a string representation of the crop
  #   min_crop (str)                : a two-character representation of the crop
  #   chosen_yr (int)               : the year to calculate the weighted average of the cliamte
  #   hist_nocc_variability (bool)  : If false, the precipitation variability will not reflect the variability_increase value. FALSE is the default.
  #   input_fd (str)                : a path to the location of the pixel files
  
  
  message(paste(Sys.time(), scen, season, tmax_or_prec, variability_increase, chosen_yr, sep = " "))
  message(paste0(Sys.time(), " Read Calendar"))
  
  
  source("01_02_hfd_agera5_by_pixel_function_MK.R")
  
  # Read country pixel
  country_pixel <- read_country_pixel(iso = iso, input_fd)
  
  # read planting calendar by season
  planting <- full_join(
    
    # Read the planting calendar for Rainfed
    read_planting_calendar(iso = iso, crop = crop, min_crop = min_crop, season = season, water = "rnfd", country_pixel = country_pixel, input_fd = input_fd) %>%
      select(c("x2", "y2", "seas", "mo", "hect")) %>% rename(hect_rnfd = hect), 
    
    # Read the planting calendar for Irrigation
    read_planting_calendar(iso = iso, crop = crop, min_crop = min_crop, season = season, water = "irri", country_pixel = country_pixel, input_fd = input_fd)%>%
      select(c("x2", "y2", "seas", "hect")) %>% rename(hect_irri = hect),
    
    # Join by location and season
    by = c("x2", "y2", "seas")
    ) %>%
    
    # Add the hectares of rainfed land and hectares of irrigated land
    mutate(hect = hect_rnfd + hect_irri) %>%
    
    # Removed the hectares of each water source (and keep only the sum)
    select(-c("hect_rnfd", "hect_irri"))
  
  
  message(paste0(Sys.time(), " Read Climate data"))
  
  # read climate data
  climate <- read_climate_file(fd = fd, iso = iso, scen = scen, variability_increase = variability_increase, tmax_or_prec = tmax_or_prec, hist_nocc_variability = hist_nocc_variability) %>%
    filter(yr == chosen_yr)
  
  message(paste0(Sys.time(), " Join Climate Data and Planting Calendar"))
  
  # Join with planting date
  season_scen_climate <- inner_join(
    ## Join Climate
    climate %>% 
      # Create x2 if xlon exists
      {
        if ("xlon" %in% colnames(.)) mutate(., x2 = round(xlon * 10, digits = 2)) %>% select(-c("xlon")) else .
      } %>% 
      # Create y2 if ylat exists
      {
        if ("ylat" %in% colnames(.)) mutate(., y2 = round(ylat * 10, digits = 2)) %>% select(-c("ylat")) else .
      }, 
    
    ## Join Planting Date
    planting %>%
    {
      # Create x2 if xlon exists
      if ("xlon" %in% colnames(.)) mutate(., x2 = round(xlon * 10, digits = 2)) %>% select(-c("xlon")) else .
    } %>% 
      
      {
        # Create y2 if ylat exists
        if ("ylat" %in% colnames(.)) mutate(., y2 = round(ylat * 10, digits = 2)) %>% select(-c("ylat")) else .
      }, by = c("x2", "y2")) %>%
    
    # Revive xlon and ylat
    mutate(xlon = x2 / 10, ylat = y2 /10)
  
  # Weighted annual average based on planting date
  message(paste0(Sys.time(), " Calculating the Annual Average during the Planting Season"))
  season_scen_climate <- weighted_by_planting_season(season_scen_climate, tmax_or_prec = tmax_or_prec)
  
  message(paste0(Sys.time(), " Weighting the Annual Average by Hectare"))
  season_scen_climate <- weighted_by_hectare(season_scen_climate, tmax_or_prec = tmax_or_prec) 
}

weighted_climate_combine <- function(iso, scen, season, chosen_yr, variable_list, variability_increase, crop, min_crop, hist_nocc_variability, input_fd){
  
  ## =========================
  ## This function applies weighted_climate_for_scen_season_yr for all cases that need their weighted average to be calculated (planting season & hectare)
  ## =========================
    
  variable_list |>
    map( ~weighted_climate_for_scen_season_yr(iso = iso, scen = scen, season = season, tmax_or_prec = .x, variability_increase = variability_increase, 
                                               crop = crop, min_crop = min_crop, chosen_yr = chosen_yr, hist_nocc_variability = hist_nocc_variability, input_fd = input_fd)) %>%
    reduce(~ full_join(.x, .y, by = c("yr", "menu_row", "seas"))) %>%
    mutate(scenario = scen, variability_increase = variability_increase)
  
}

###########
### 3. Graphing Function
###########
temp_prec_density_function <- function(df){
  
  ## =========================
  ## This function graphs the 2d Density plot for tmax and pr
  ## =========================
  
  # Save the variables
  season <- unique(df$seas)
  yr <- unique(df$yr)
  scenario <- unique(df$scenario)
  variability_increase <- unique(df$variability_increase)
  
  # Define subtitle text
  subtitle_text <- ifelse(season == "",
         paste0("Distribution of Climate in the year ", yr, " for Scenario ", scenario, "\n", 
                "Precipitation Variability: ", (variability_increase-1)*100, "%"),
         paste0("Distribution of Climate in the year ", yr, " during the ", season," season", " for Scenario ", scenario, "\n", 
                "Precipitation Variability: ", (variability_increase-1)*100, "%"))
  
  df %>%
    ggplot()+
    
    # Build a 2d Density Plot
    geom_density_2d(aes(x = weighted_txyr, weighted_pryr, color = after_stat(level)), adjust = 2)+
    
    # Color scale of the graph lines
    scale_color_viridis_b(name = "Density", limits = c(0, 0.1))+
    
    # Scale of the x and y axis
    scale_x_continuous(
      breaks = breaks_width(0.5),
      minor_breaks = breaks_width(0.25))+
    
    scale_y_continuous(
      breaks = breaks_width(50),
      minor_breaks = breaks_width(25))+
    
    theme_bw()+
    
    # Labels
    labs(
      x = "Weighted Average Maximum Temperature During the Season",
      y = "Weighted Average Precipitation During the Season",
      subtitle = subtitle_text
    ) +
    
    theme(
      text = element_text(family = "Times"),
    )
}
