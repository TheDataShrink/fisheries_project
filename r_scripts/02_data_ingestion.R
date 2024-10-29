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
library(ggplot2)
# Initialize logging
# fs::dir_create("logs")
logger::log_threshold(DEBUG)
logger::log_appender(appender_file("logs/data_ingestion.log"))


source("r_scripts/03_connect_to_db.R")
source("r_scripts/04_create_bronze_table.R")
source("r_scripts/05_process_file.r")
source("r_scripts/07_write_to_db.R")

# Main execution
main <- function() {
  logger::log_info("Starting data ingestion process")
  
  # Create logs directory if it doesn't exist
  if (!dir.exists("logs")) dir.create("logs")
  
  # Connect to database
  con <- connect_to_db(db_path = "data/fisheries.duckdb")
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

query <- "DROP TABLE BRONZE_CATCH_DATA"

# Test the table was created.
con <- connect_to_db(db_path = "data/fisheries.duckdb")
bronze_tbl <- dplyr::tbl(con, "BRONZE_CATCH_DATA") |> dplyr::collect()
initialize_control_table(con = con)


# bronze_tbl |>
#   count(NUMBER_OF_LIVE_SPECIMENS,NUMBER_OF_DEAD_SPECIMENS) |>
#   mutate(TOTAL_SPECIMENS = NUMBER_OF_LIVE_SPECIMENS + NUMBER_OF_DEAD_SPECIMENS) |>
#   mutate(PRC = NUMBER_OF_LIVE_SPECIMENS/TOTAL_SPECIMENS) |>
#   filter(!is.na(PRC)) |>
#   ggplot(aes(PRC)) +
#   geom_histogram()

# Check what tables exist
# tables <- list_tables(con)
#   
# # Get row counts
# for (table in tables) {
#   count <- get_table_count(con, table)
#   print(sprintf("Table %s has %d rows", table, count))
# }    
# Create tables

transform_to_silver_catch_data(con,source_table = "BRONZE_CATCH_DATA", 
                               target_table = "SILVER_CATCH_DATA")

cleaned_data <- 
  bronze_tbl |> 
  filter(!is.na(LOG_NO) & !is.na(LOCATION_NAME) & NUMBER_OF_LIVE_SPECIMENS >= 0)


# Define the directory containing data files
data_directory <- "raw_data/csv/"

# List all CSV files in the directory
file_list <- list.files(path = data_directory, pattern = "\\.csv$", full.names = TRUE)


# Function to insert a control record
insert_control_record <- function(con, process_control_uid, file_name, file_path, received_datetime, data_date) {
  query <- "
  INSERT INTO PROCESS_CONTROL
  (PROCESS_CONTROL_UID, FILE_NAME, FILE_PATH, RECEIVED_DATETIME, STATUS, DATA_DATE)
  VALUES (?, ?, ?, ?, 'RECEIVED', ?);
  "
  dbExecute(con, query, params = list(process_control_uid, file_name, file_path, received_datetime, data_date))
}



dbExecute(con, query)

# Function to update the control table
update_control_table <- function(con, process_control_uid, status, step = NULL, row_count = NULL) {
  query <- "
  UPDATE PROCESS_CONTROL
  SET STATUS = ?,
      STEP = ?,
      PROCESSED_DATETIME = CURRENT_TIMESTAMP,
      ROW_COUNT = ?
  WHERE PROCESS_CONTROL_UID = ?;
  "
  dbExecute(con, query, params = list(status, step, row_count, process_control_uid))
}

# Call the updated function
# Generate a UUID
process_control_uid <- UUIDgenerate()

# Loop over each file
for (file_path in file_list) {
  tryCatch({
    # Generate a UUID for each file
    process_control_uid <- UUIDgenerate()
    
    # Get the file name from the file path
    file_name <- basename(file_path)
    
    # Record the received datetime
    received_datetime <- Sys.time()
    
    # Set data_date (you can extract from file if possible)
    data_date <- as.Date("2023-05-01")
    
    # Insert control record
    insert_control_record(
      con = con,
      process_control_uid = process_control_uid,
      file_name = file_name,
      file_path = file_path,
      received_datetime = received_datetime,
      data_date = data_date
    )
    
    # Read the data
    catch_data <- readr::read_csv(file_path)
    
    # Add metadata
    catch_data$LOADING_DATETIME <- Sys.time()
    catch_data$SOURCE_FILE <- file_name
    catch_data$PROCESSING_STATUS <- "UNPROCESSED"
    
    # Write to bronze table
    dbWriteTable(con, "BRONZE_CATCH_DATA", catch_data, append = TRUE)
    
    # Update control table to success
    row_count <- nrow(catch_data)
    update_control_table(
      con = con,
      process_control_uid = process_control_uid,
      status = 'SUCCESS',
      step = 'Ingestion',
      row_count = row_count
    )
    
  }, error = function(e) {
    # Update control table to failed
    update_control_table(
      con = con,
      process_control_uid = process_control_uid,
      status = 'FAILED',
      step = 'Ingestion',
      row_count = 0
    )
    message(paste("Failed to process", file_name, ":", e$message))
  })
}

# Close the database connection
dbDisconnect(con)
