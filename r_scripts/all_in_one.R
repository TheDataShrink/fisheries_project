# Load necessary libraries
library(DBI)
library(duckdb)
library(readxl)
library(dplyr)
library(lubridate)
library(logger)
library(glue)

# Set up logger
log_appender(appender_console)
log_threshold(INFO)

# Establish database connection
con <- dbConnect(duckdb::duckdb(), dbdir = "data/fisheries.duckdb", read_only = FALSE)

# Helper function: Write data to database with error handling and table creation
write_to_db <- function(con, data, table_name = 'BRONZE_CATCH_DATA', create_if_missing = TRUE) {
  tryCatch({
    # Check if table exists
    if (!dbExistsTable(con, table_name) && create_if_missing) {
      dbWriteTable(
        con, 
        name = table_name, 
        value = data,
        row.names = FALSE
      )
      log_info(glue("Created new table {table_name} with {nrow(data)} rows"))
    } else {
      dbWriteTable(
        con, 
        name = table_name, 
        value = data, 
        append = TRUE, 
        row.names = FALSE
      )
      log_info(glue("Appended {nrow(data)} rows to {table_name}"))
    }
    return(TRUE)
  }, error = function(e) {
    log_error(glue("Failed to write to database: {e$message}"))
    return(FALSE)
  })
}

# Function: Ingest data into the bronze layer
ingest_bronze_data <- function(con, data_dir = "raw_data") {
  tryCatch({
    # Get list of all Excel files in the data directory
    data_files <- list.files(data_dir, pattern = '\\.xlsx$', full.names = TRUE, recursive = TRUE)
    
    if (length(data_files) == 0) {
      log_info("No data files found for ingestion.")
      return(FALSE)
    }
    
    for (file in data_files) {
      # Extract metadata from file path
      file_name <- basename(file)
      path_parts <- strsplit(file, .Platform$file.sep)[[1]]
      data_date <- as.Date(path_parts[length(path_parts) - 1])
      country <- strsplit(file_name, "_")[[1]][1]
      
      # Read data
      data <- read_excel(file)
      
      # Add metadata columns
      data$DATA_DATE <- data_date
      data$COUNTRY <- country
      data$LOADING_DATETIME <- Sys.time()
      data$SOURCE_FILE <- file_name
      data$PROCESSING_STATUS <- "UNPROCESSED"
      data$ID <- 1:nrow(data) 
      
      # Standardize column names to uppercase
      names(data) <- toupper(gsub(' ', '_', names(data)))
      
      # Write to bronze table
      success <- write_to_db(con, data, table_name = "BRONZE_CATCH_DATA")
      
      if (success) {
        log_info(glue("Ingested data from {file_name}"))
      } else {
        log_error(glue("Failed to ingest data from {file_name}"))
      }
    }
    return(TRUE)
  }, error = function(e) {
    log_error(glue("Data ingestion failed: {e$message}"))
    return(FALSE)
  })
}

# Function: Transform bronze catch data into silver layer
transform_to_silver_catch_data <- function(con, source_table = "BRONZE_CATCH_DATA", 
                                           target_table = "SILVER_CATCH_DATA") {
  tryCatch({
    # Read data from the bronze table
    data_bronze <- dbReadTable(con, source_table)
    
    # Filter unprocessed data
    data_bronze <- data_bronze[data_bronze$PROCESSING_STATUS == "UNPROCESSED", ]
    
    if (nrow(data_bronze) == 0) {
      log_info("No unprocessed data found in bronze table.")
      return(0)
    }
    
    # Data transformations in R
    # Data transformations in R
    data_silver <-
      data_bronze |>
      # Parse dates with multiple formats
      mutate(PARSED_DATE = lubridate::parse_date_time(DATE,orders = c("%d-%b-%Y","%m/%d/%Y", "%m-%d-%y", "%Y-%m-%d")),
             VESSEL_NAME = toupper(HOLDERS_NAME),
             LICENSE_NUMBER = LICENCE_NO,
             LOG_NUMBER = as.integer(LOG_NO),
             PAGE_NUMBER = as.integer(PAGE_NO),
             COUNTRY = toupper(COUNTRY),
             SPECIES = toupper(SPECIES),
             COLLECTION_TIME_HOURS = as.numeric(COLLECTION_TIME_HOURS),
             NUMBER_OF_DEAD_SPECIMENS = as.integer(NUMBER_OF_DEAD_SPECIMENS),
             NUMBER_OF_LIVE_SPECIMENS = as.integer(NUMBER_OF_LIVE_SPECIMENS),
             LOCATION_NAME = toupper(LOCATION_NAME),
             WEATHER_CONDITION = toupper(WEATHER_CONDITION),
             UPDATED_DATETIME = Sys.time()
      ) |>
      filter(!is.na(PARSED_DATE)) |>
      select(
        CATCH_DATA_UID = ID,
        VESSEL_NAME,
        LICENSE_NUMBER,
        LOG_NUMBER,
        PAGE_NUMBER,
        NON_COLLECTING_CODE,
        COUNTRY,
        SPECIES,
        CATCH_DATE = PARSED_DATE,
        COLLECTION_TIME_HOURS,
        NUMBER_OF_DEAD_SPECIMENS,
        NUMBER_OF_LIVE_SPECIMENS,
        LOCATION_NAME,
        WEATHER_CONDITION,
        LOADING_DATETIME,
        SOURCE_FILE,
        UPDATED_DATETIME
      )
    
    
    # Write to silver table
    success <- write_to_db(con, data_silver, table_name = target_table)
    
    if (success) {
      # Update processing status in bronze table
      ids_processed <- data_silver$CATCH_DATA_UID
      # Prepare IDs for SQL IN clause
      ids_string <- paste(ids_processed, collapse = ",")
      update_query <- sprintf(
        "UPDATE %s SET PROCESSING_STATUS = 'PROCESSED' WHERE ID IN (%s)",
        source_table, ids_string
      )
      dbExecute(con, update_query)
      
      row_count <- nrow(data_silver)
      log_info(glue("Successfully transformed {row_count} rows to silver layer"))
      return(row_count)
    } else {
      log_error("Failed to write data to silver table")
      return(0)
    }
  }, error = function(e) {
    log_error(glue("Failed to transform data to silver layer: {e$message}"))
    return(0)
  })
}

source(file = "r_scripts/10_create_silver_aggregations.R")

# Main script execution
main <- function() {
  # Step 1: Ingest data into bronze layer
  ingestion_success <- ingest_bronze_data(con)
  
  if (ingestion_success) {
    log_info("Data ingestion completed successfully.")
  } else {
    log_error("Data ingestion failed.")
    return(NULL)
  }
  
  # Step 2: Transform data to silver layer
  rows_transformed <- transform_to_silver_catch_data(con)
  
  if (rows_transformed > 0) {
    log_info(glue("{rows_transformed} rows transformed to silver layer."))
  } else {
    log_error("No data transformed to silver layer.")
    return(NULL)
  }
  
  # Step 3: Create or update silver aggregations
  aggregation_success <- create_silver_aggregations(con)
  
  if (aggregation_success) {
    log_info("Silver layer aggregations created/updated successfully.")
  } else {
    log_error("Failed to create/update silver layer aggregations.")
    return(NULL)
  }
  
  # Close the database connection
  dbDisconnect(con)
  
  log_info("Data processing pipeline completed successfully.")
}

# Run the main function
main()
