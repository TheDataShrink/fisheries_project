# Load required libraries
library(DBI)
library(duckdb)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(tidyr)
library(logger)

# Initialize logging
log_threshold(DEBUG)
log_appender(appender_file("logs/data_ingestion.log"))


source("r_scripts/connect_to_db.r")
source("r_scripts/create_bronze_table.r")
source("r_scripts/process_file.r")
source("r_scripts/write_to_db.r")

# Main execution
main <- function() {
  logger::log_info("Starting data ingestion process")
  
  # Create logs directory if it doesn't exist
  if (!dir.exists("logs")) dir.create("logs")
  
  # Connect to database
  con <- connect_to_db()
  if (is.null(con)) {
    logger::log_error("Exiting due to database connection failure")
    return(FALSE)
  }
  
  # Create bronze table if needed
  create_bronze_table(con)
  
  # Get list of files
  data_files <- list.files('raw_data/', 
                           pattern = '\\.xlsx$', 
                           full.names = TRUE, 
                           recursive = TRUE)
  
  log_info("Found {length(data_files)} files to process")
  
  # Process each file
  for (file in data_files) {
    data <- process_file(file)
    
    if (!is.null(data)) {
      success <- write_to_db(con, data)
      if (!success) {
        logger::log_error("Failed to write data from {file} to database")
      }
    }
  }
  
  # Clean up
  dbDisconnect(con)
  logger::log_info("Completed data ingestion process")
}

# Run the main function
main()

# Test the table was created.
tbl(con, "BRONZE_CATCH_DATA") |>
  collect()
