##############################
##
## Confirm the Quality of the Yield Generation & Regenerate it
## by MinAh Kim
## Oct 22, 2024
##
##############################
yield_generator_quality <- function(iso,
                                    crop, 
                                    min_crop,
                                    season,
                                    scen,
                                    water,
                                    irri_rn_limit,
                                    sample_beg,
                                    sample_end,
                                    variability_increase = 0,
                                    hist_nocc_variability = hist_nocc_variability,
                                    nitr = 40,
                                    maxylr = 20000,
                                    co2_constant =370,
                                    input_fd = "/Volumes/TOSHIBA EXT/CIAT/Data/Input_data/",
                                    fd = "/Volumes/T9/"){
  
  
  #####################################################
  ## Check the Quality of the Yields & Regenerate it when the issue arises
  #####################################################
  
  
  tryCatch(
    {
      
      # Read the csv of interest
      read_csv(paste0(fd, iso, "/", iso, "_", water, "_", min_crop, "_", scen, "_", season, "_pixel_", sample_beg, "to", sample_end, ".csv"),
               show_col_types = FALSE)
    
    }, warning = function(w){
      
      # If warning arises
      cat(paste0("Warning in between Sample ", sample_beg, " - ", sample_end))
      
      # Regenerate the yield
      source("01_01_hfd_agera5_by_pixel_MK.R")
      yield_generator(
          iso = iso,
          crop = crop, 
          min_crop = min_crop,
          season_list = season, 
          scenario_list = scen,
          water_list = water,
          irri_rn_limit = irri_rn_limit,
          sample_beg_list = sample_beg,
          sample_end_list = sample_end,
          hist_nocc_variability = hist_nocc_variability,
          variability_increase = variability_increase,
          nitr = nitr,
          maxylr = maxylr,
          co2_constant =co2_constant,
          input_fd = input_fd,
          fd = fd
        )
      
      
    }, error = function(e){
      
      # If the error arises
      cat(paste0("Error in between Sample ", sample_beg, " - ", sample_end))
      source("02_03_hfd_agera5_by_pixel_MK.R")
      yield_generator(
        iso = iso,
        crop = crop, 
        min_crop = min_crop,
        season_list = season, 
        scenario_list = scen,
        water_list = water,
        irri_rn_limit = irri_rn_limit,
        sample_beg_list = sample_beg,
        sample_end_list = sample_end,
        hist_nocc_variability = hist_nocc_variability,
        variability_increase = variability_increase,
        nitr = nitr,
        maxylr = maxylr,
        co2_constant =co2_constant,
        input_fd = input_fd,
        fd = fd
      )
      
    }, finally = flush.console()
    
)
  
}
