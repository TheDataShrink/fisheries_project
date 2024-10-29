# Function: Create or update silver layer aggregation tables using R
create_silver_aggregations <- function(con) {
  tryCatch({
    # Load necessary packages
    library(dplyr)
    library(lubridate)
    library(DBI)
    library(logger)
    library(glue)
    
    # Read data from SILVER_CATCH_DATA
    silver_data <- dbReadTable(con, "SILVER_CATCH_DATA")
    
    if (nrow(silver_data) == 0) {
      log_info("No data found in SILVER_CATCH_DATA.")
      return(FALSE)
    }
    
    # Ensure date columns are in Date format
    silver_data$CATCH_DATE <- as.Date(silver_data$CATCH_DATE)
    
    # 1. Daily catch summary
    daily_catch <- silver_data %>%
      mutate(CATCH_DAY = as.Date(CATCH_DATE)) %>%
      group_by(CATCH_DAY, SPECIES, FISHING_AREA = LOCATION_NAME) %>%
      summarise(
        NUMBER_OF_CATCHES = n(),
        TOTAL_SPECIMENS = sum(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        UNIQUE_VESSELS = n_distinct(VESSEL_NAME),
        .groups = "drop"
      )
    
    # Write daily_catch to database
    write_to_db(con, daily_catch, table_name = "SILVER_DAILY_CATCH")
    
    # 2. Monthly vessel summary
    monthly_vessel <- silver_data %>%
      mutate(
        CATCH_MONTH = format(CATCH_DATE, "%Y-%m"),
        CATCH_DAY = as.Date(CATCH_DATE)
      ) %>%
      group_by(CATCH_MONTH, VESSEL_NAME, FISHING_AREA = LOCATION_NAME) %>%
      summarise(
        FISHING_DAYS = n_distinct(CATCH_DAY),
        SPECIES_CAUGHT = n_distinct(SPECIES),
        TOTAL_SPECIMENS = sum(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        AVG_SPECIMENS_PER_DAY = mean(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Write monthly_vessel to database
    write_to_db(con, monthly_vessel, table_name = "SILVER_MONTHLY_VESSEL_STATS")
    
    # 3. Species distribution summary
    species_summary <- silver_data %>%
      group_by(SPECIES, FISHING_AREA = LOCATION_NAME) %>%
      summarise(
        TOTAL_CATCHES = n(),
        TOTAL_SPECIMENS = sum(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        AVG_SPECIMENS_PER_CATCH = mean(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        MIN_SPECIMENS_PER_CATCH = min(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        MAX_SPECIMENS_PER_CATCH = max(NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS, na.rm = TRUE),
        ACTIVE_DAYS = n_distinct(as.Date(CATCH_DATE)),
        UNIQUE_VESSELS = n_distinct(VESSEL_NAME),
        .groups = "drop"
      )
    
    # Write species_summary to database
    write_to_db(con, species_summary, table_name = "SILVER_SPECIES_SUMMARY")
    
    log_info("Successfully created silver layer aggregation tables using R.")
    return(TRUE)
    
  }, error = function(e) {
    log_error(glue("Failed to create silver aggregations: {e$message}"))
    return(FALSE)
  })
}
