###########
#
#     Wrapper File that Generates Yields
#     September, 10, 2024 
#     MinAh's Version
#
###########

##

###########
### 0. Set the Variable and Library
###########

# ============
library(tidyr)
library(dplyr)
library(readxl)
library(purrr)
library(quarto)
library(stringr)
library(pbapply)
# ============

# This wrapper assumes two file directories based on MinAh's edition of codes.
# Please set the correct file director based on these two definition.

# ============
# 1.
# input_fd: the directory where the input files are located. It is usually a file not specific to a single country
#   (e.g. agera5_iso_gadm2p7.csv, random_climate.csv, agera5_prec_parameters_mo_1.csv, agera5_prec_resid_mo_1.csv)
input_fd <- "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/"
# ============

# ============
# 2.
# fd: the directory where the generated synthetic climate and the yield data will be saved. There are also some country-specific data generated in midway.
# Usually, it will be saved to a file that has iso as a name 
#   (e.g. GHA_prcp_hist&nocc.csv, GHA_interannual_var_prec_sample.rds)
fd <- "/Volumes/T9/"
# ============

# ============
# 3.
# Set the Country iso to generate
iso <- "ETH"

# ============

# ============
# 4.
# Set the variable for the variability increase
# When 1, there is no variability increase. Else, the value is multiplied with the variability increase 
# (e.g. when variability increase is 1.14, 1.14 is multiplied and therefore there is an 14% increase of original value)
variability_increase <- 1

# if TRUE,the variability_increase will be applied for hist&nocc scenario
hist_nocc_variability <- FALSE
# ============

# ============
# 5.
# Set the variable for the crop
crop <- "teff"

# min_crop is determined by crop
min_crop <- case_when(
  crop == "grou" ~ "gd",
  crop == "maiz" ~ "mz",
  crop == "teff" ~ "tf"
)

# full_crop is determined by crop
full_crop <- case_when(
  crop == "grou" ~ "Groundnut",
  crop == "maiz" ~ "Maize",
  crop == "teff" ~ "Teff"
)
# ============

# ============
# 6.
# Set the scenarios of interest
scen_list <- c("hist&nocc","BASECOV")
# ============

# ============
# 7. 
# Set the Water Source of interest
# Always keep Rainfed first to prevent confusion in the visualization
water_list <- c("rnfd" , "irri")
# ============

# ============
# 8. 
# Set the Season of interest (if there is)
season_list <- c("meher", "belg")
# ============

# ============
# 9. 
# Irrigation Minimum Water
# Format should be c(month_number = amount of rainfall, ....)
irri_rn_limit <- case_when(
  crop == "maiz" ~ c("1" = 100, "2" = 100, "3" = 100, "4" = 100),
  crop == "teff" ~ c("1" = 100, "2" = 100, "3" = 25, "4" = 100)
  )
# ============

# ============
# 10.
# Set up Nitrogen level for the yield emulator 
nitr <- 40
# ============

# ============
# 11.
# maxylr
# Set maximum yield that the emulator will record
maxylr <- 20000
# ============

# ============
# 12.
# co2_constant
# Set the CO2 constant used for the yield emulator
co2_constant <- 370
# ============

# ============
# 13.
# Level of Aggregation
# Set the Unit of Aggregation
# Option: national, regional, aez5
aggregation <- "aez5"
# ============


###########
### 1. Generate Yield Data
###########

# When it says "No such file or directory", always set the working directory to according to the location of the file
# Import Function 
source("01_01_hfd_agera5_by_pixel_MK.R")

# Run the function
yield_generator(iso = iso,
                crop = crop,
                min_crop = min_crop,
                season_list = season_list,
                scenario_list = scen_list,
                water_list = water_list,
                irri_rn_limit = irri_rn_limit,
                hist_nocc_variability = hist_nocc_variability,
                variability_increase = variability_increase,
                nitr = nitr,
                maxylr = maxylr,
                co2_constant = co2_constant)


###########
### 2. Rescaling Factor 
###########

# Read the excel file that has the factor
rescaling_factor <- readxl::read_excel(paste0(fd, "Rescaling factor/rescale_crops_20240528.xlsx"), sheet = "rescale_crops")
rescaling_factor <- rescaling_factor[!is.na(rescaling_factor$iso),]
rescale <- as.numeric(rescaling_factor[(rescaling_factor$iso == iso) & (rescaling_factor$commod == min_crop), "scale"])

# Calculate the Rescaling Factor When It Does not Exist
if(is.na(rescale)){
  
  source("01_03_hfd_agera5_by_pixel_quality_check_MK.R")
  
  # Check the quality of data before calculating the rescaling factor
  expand_grid(seas = season_list, water_one = water_list) %>%
    pmap(\(seas, water_one) {
      
      # Ensure there is no corrupt data
      list(x = seq(1, 901, by = 100), y = seq(100, 1000, by = 100)) %>%
        pwalk(\(x, y) yield_generator_quality(iso = iso,
                                             crop = crop, 
                                             min_crop = min_crop,
                                             season = seas,
                                             scen = "hist&nocc",
                                             water = water_one,
                                             irri_rn_limit = irri_rn_limit,
                                             sample_beg = x,
                                             sample_end = y,
                                             variability_increase = variability_increase,
                                             hist_nocc_variability = hist_nocc_variability,
                                             nitr = nitr,
                                             maxylr = maxylr,
                                             co2_constant =co2_constant,
                                             input_fd = input_fd,
                                             fd = fd))
    })
  
  # Calculate the rescaling factor after the data qualtiy check
  source("03_01_rescaling_calculation_MK.R")
  rescale <- calculate_rescaling_factor(iso = iso, crop = crop, min_crop = min_crop, season_list = c("meher", "belg"), water_list = c("rnfd", "irri"), irri_rn_limit = irri_rn_limit)
}

###########
### 3.Aggregation & Rescaling
###########

#Aggregate in National/Subnational Unit Level
source("02_00_reading_distributions_aggregations_scalings_MK.R")

# Ensure there is no corrupt data
source("01_03_hfd_agera5_by_pixel_quality_check_MK.R")

# Aggregate for each GHG scenarios and seasons
pbapply(expand_grid(scen_list, season_list), 1, function(x){
  
    # Read the variables passed into the lambda function  
    scen_list <- x[1]
    season_list <- x[2]
    
    # For all sample
    expand_grid(data.frame(list(x = seq(1, 901, by = 100), y = seq(100, 1000, by = 100))), water_one = c("rnfd", "irri")) %>%
      
      # Check the yield quality
      pwalk(\(x, y, water_one) yield_generator_quality(iso = iso,
                                                       crop = crop, 
                                                       min_crop = min_crop,
                                                       season = season_list,
                                                       scen = scen_list,
                                                       water = water_one,
                                                       irri_rn_limit = irri_rn_limit,
                                                       sample_beg = x,
                                                       sample_end = y,
                                                       variability_increase = variability_increase,
                                                       hist_nocc_variability = hist_nocc_variability,
                                                       nitr = nitr,
                                                       maxylr = maxylr,
                                                       co2_constant =co2_constant,
                                                       input_fd = input_fd,
                                                       fd = fd))
    
    # If the hist_nocc_variability is FALSE, the variability_increase does not apply for the hist&nocc case
    variability_increase_case <- ifelse((scen_list == "hist&nocc") & hist_nocc_variability == FALSE, 1, variability_increase)
    
    # Aggregate the rescaled value based on the variability increase value 
    # Save it to csv
    
      if(variability_increase_case == 1){
        
        write.csv(aggregation_rescaling(iso = iso, crop = crop, min_crop = min_crop, scen = scen_list, season = season_list, variability_increase = variability_increase_case, 
                                        hist_nocc_variability = hist_nocc_variability, rescale_value = rescale, aggregation = aggregation, input_fd = input_fd, fd = fd), 
                  paste0(fd, iso, "/", iso, "_rescaled_", crop, "_", scen_list, "_", season_list, "_", aggregation, ".csv"))
        
      } else{
        
        write.csv(aggregation_rescaling(iso = iso, crop = crop, min_crop = min_crop, scen = scen_list, season = season_list, variability_increase = variability_increase_case, 
                                        hist_nocc_variability = hist_nocc_variability, rescale_value = rescale, aggregation = aggregation, input_fd = input_fd, fd = fd), 
                  paste0(fd, iso, "/", iso, "_rescaled_", crop, "_", "var_", variability_increase_case, "_", scen_list, "_", season_list, "_", aggregation, ".csv"))
        
      }

    return(paste0("Sucess : ", scen_list, " ", season_list))
  }
)

   

###########.                                                             
### 4. Generate Data Visualization
###########

# File name
file_name <- case_when(
  aggregation == "national" & variability_increase == 1  ~  paste0(iso, "_", crop, "_Distribution_Report_", Sys.Date(), ".docx"),
  aggregation == "national" & variability_increase != 1 ~ paste0(iso, "_", crop, "_Distribution_Report_Variability_", variability_increase, "_", Sys.Date(), ".docx"),
  aggregation != "national" & variability_increase == 1  ~  paste0(iso, "_", crop, "_Distribution_Report_Regional_", Sys.Date(), ".docx"),
  aggregation != "national" & variability_increase != 1 ~ paste0(iso, "_", crop, "_Distribution_Report_Variability_", variability_increase, "_Regional_", Sys.Date(), ".docx"),
)

# Render Quarto
quarto_render("03_01_ethiopia_yield_analysis.qmd", 
              execute_params = list(
                "crop" = crop,
                "full_crop" = full_crop,
                "min_crop" = min_crop,
                "variability_increase" = variability_increase,
                "season_list" = str_flatten(season_list, collapse = ", "),
                "scenario_list" = str_flatten(scen_list, collapse = ", "),
                "water_list" = str_flatten(water_list, collapse = ", "),
                "aggregation" = aggregation,
                "hist_nocc_variability" = FALSE,
                "include_climate" = FALSE
              ), 
              output_file = file_name, 
              output_format = "docx")

=