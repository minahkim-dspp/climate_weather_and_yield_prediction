library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(pbapply)
library(foreign)


#######################
## 1 & 2 Function for Step 1 and 2 (national_aggregation)
#######################

read_pixel <- function(iso, scen, min_crop, water_source, variability_increase, hist_nocc_variability, season = "", fd = "/Volumes/T9/"){
  
  #######################
  ## 1. Read Data at Pixel 
  #######################
  
  #### Read crop yield at pixel
  
  ## Parameter:
  #   iso (str)                   : A three letter code that represents a country
  #   scen (str)                  : Greenhouse emission scenario. Currently assumed to be either "hist&nocc", "PARIS_1p5C", or "BASECOV"
  #   min_crop (str)              : A two letter abbreviation to represent the crop. Used to read the pixel level csv file.
  #   water_source(str)           : String that represents the water source of the field. It could be either "rnfd" for rainfed or "irri" for irrigation.
  #   variability_increase(dbl)   : A float value that is multiplied in the precipitation value to represent the increase of precipitation based on the increase in temperature. 
  #   hist_nocc_variability(bool) : A boolean value that determines if the variability increase will affect the hist&nocc case. Default is FALSE. 
  #                                 If set as TRUE, it assumes that the previous code calculates the precipitation of hist&nocc multiplied by the variability_increase.
  
  ## Return : 
  #   crop_yield (dataframe): dataframe with the column xlon, ylat, yr, nitr, co2, hect, yld, overmax, prod, sim_num (may change based on the change in code when generating the pixel level output)
  
  # If Variability increase should not affect "hist&nocc" cases, return variability_increase to 1
  variability_increase_case <- ifelse((scen == "hist&nocc") & (hist_nocc_variability == FALSE), 1, variability_increase)
  
  # Create the part of the file name that has the scenario, water source, crop and scenario
  file_name_root <- ifelse(variability_increase_case!= 1, 
                           paste(c(iso, water_source, min_crop, "var", variability_increase_case, scen, season, "pixel"), collapse = "_"),
                           paste(c(iso, water_source, min_crop, scen, season, "pixel"), collapse = "_"))
  
  # If the according pixel file exists:
  if(length(list.files(path= paste0(fd, "/", iso), pattern= paste0(paste(c(iso, water_source, min_crop, scen, season, "pixel"), collapse = "_"), "_", "\\d+", "to", "\\d+",".csv")))>0){
    
    crop_yield <- list(x = seq(1, 901, by = 100), y = seq(100, 1000, by = 100)) %>%
      
      # Read the pixel level csv file
      pmap(\(x, y) read.csv(paste0(fd, iso, "/", file_name_root, "_", x, "to", y,".csv")) %>% select(-"X")) %>%
      
      # Combine all files into one dataframe
      bind_rows() %>% 
      
      # rename menu_row as sim_num
      rename(sim_num = menu_row) %>% 
      
      # arrange in an ascending order based on xlon and ylat
      arrange(xlon, ylat)
    
    return(crop_yield)
    
  } else {
    
    return(data.frame(matrix(ncol = 5, nrow = 0, dimnames=list(NULL, c("sim_num", "yr", "hect", "prod", "yld")))))
    
  }
}

national_aggregation <- function(crop_yield, rescale_value = 1){
  
  #######################
  ## 2-1. Rescale in Pixel Level & Aggregate to the national level  
  #######################
  ## Parameter : dataframe resulted from read_pixel
  
  ## Return: dataframe with sim_num (sample number), yr (year), hect (hectare), prod (production), yld (yield). 
  
  # National Level Aggregation
  national_aggregation <- crop_yield %>% 
    
    # Multiply yld to rescaling value in pixel level
    # Multiply prd to rescaling value in pixel level
    mutate(yld = yld * rescale_value,
           prod = prod * rescale_value) %>%
    
    # Group by sim_num and year
    group_by(sim_num, yr) %>%
    
    # Summarize by summing all the values in the group
    summarize(hect_sum = sum(hect), prod_sum = sum(prod)) %>%
    
    # Calculate the national aggregation of the yield
    mutate(yld = prod_sum/hect_sum) %>%
    
    # Return the name of hect_sum and prod_sum to hect and prod to avoid confusion
    rename(hect = hect_sum, prod = prod_sum)
  
  return(national_aggregation)
}


regional_aggregation <- function(crop_yield, aggregation, rescale_value = 1, input_fd =  "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/"){
  
  #######################
  ## 2-2. Rescale in Pixel Level & Aggregate to the regional level
  #######################
  ## Parameter: 
  #   crop_yield (dataframe) : dataframe that is the result of read_pixel
  #   aggregation (string) : subregional aggregation level. It could be either "regional" or "aez5". Any other result will return an error. 
  
  # Read the dbf file
  pixel_region <- read.dbf(paste0(input_fd, "eth_agera5_regions_agroecozones.dbf")) %>% 
    mutate(x2 = round(xlon*10, digits = 0), y2 = round(ylat*10, digits = 0)) %>%
    select(c("x2", "y2", "NAME_1", "aez5")) %>%
    rename(region = NAME_1) %>%
    distinct()
  
  # National Level Aggregation
  regional_aggregation <- left_join(crop_yield %>% mutate(x2 = round(xlon*10, digits = 0), y2 = round(ylat*10, digits = 0)), 
                                    pixel_region, 
                                    by = c("x2", "y2")) %>%
    
    # Multiply yld to rescaling value in pixel level
    # Multiply prd to rescaling value in pixel level
    mutate(yld = yld * rescale_value,
           prod = prod * rescale_value) %>%
    
    {
      if(aggregation == "regional") group_by(., region) else if (aggregation == "aez5") group_by(., "aez5")
    } %>%
    group_split() %>%
    map(\(df) {
      
      df %>%
        {
          # Group by sim_num and year
          if (aggregation == "regional") group_by(.data = ., sim_num, yr, region) else if (aggregation == "aez5") group_by(.data = ., sim_num, yr, aez5)
          
        } %>%
        # Summarize by summing all the values in the group
        summarize(hect_sum = sum(hect), prod_sum = sum(prod)) %>%
        # Calculate the national aggregation of the yield
        mutate(yld = prod_sum/hect_sum) %>%
        # Return the name of hect_sum and prod_sum to hect and prod to avoid confusion
        rename(hect = hect_sum, prod = prod_sum) 
      
    }) %>%
    bind_rows()
  
  return(regional_aggregation)
}


#######################
## 3. Functions for all cases in different ways
#######################

name_change_for_all_scen <- function(df, crop, water){
  
  # Change the name of the variables to be mergable
  for(var in c("hect", "yld", "prod")){
    names(df)[names(df) == var] <- paste(c(var, crop, water), collapse ="_")
  }
  return(df)
  
}


#####################
#
## Main Function
#
#####################

aggregation_rescaling <- function(iso, crop, min_crop, scen, season = "", variability_increase, hist_nocc_variability, rescale_value, aggregation, input_fd, fd){
  
  cat("=======================")
  cat("National Aggregation")
  cat("=======================")
  
  
  df <- lapply(c("rnfd", "irri"), function(x) {
    
    # Read the pixel file that fits the condition
    read_pixel(iso = iso, scen = scen, water_source = x, min_crop = min_crop, 
               variability_increase = variability_increase, hist_nocc_variability = hist_nocc_variability, season = season, fd = fd) %>% 
      {
        
        # Aggregate in either national/regional level based on the aggregation variable
        if(aggregation == "national") national_aggregation(crop_yield = ., rescale_value = rescale_value) else regional_aggregation(crop_yield = ., aggregation = aggregation, rescale_value = rescale_value, input_fd = input_fd)
        
      } %>%
      
      # Change the name of the dataframe to fit the format
      name_change_for_all_scen(crop = crop, water = x)
    
  }) %>%
    {
      
      # Join the aggregated result for rainfed and the irrigation 
      if (aggregation == "national") reduce(., full_join, by = c("sim_num", "yr")) else if(aggregation == "regional") reduce(., full_join, by = c("sim_num", "yr", "region")) else if(aggregation == "aez5") reduce(., full_join, by = c("sim_num", "yr", "aez5")) 
    } %>%
    
    # Change the column names
    mutate(scenario  = scen, planting_season = season)
  
  return(df)
  
}
