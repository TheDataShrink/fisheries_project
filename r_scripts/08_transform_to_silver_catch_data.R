#' Transform bronze catch data into silver layer
#' @param con DuckDB connection
#' @param source_table Name of the source (bronze) table
#' @param target_table Name of the target (silver) table
#' @return Number of rows processed
#' @export
transform_to_silver_catch_data <- function(con, source_table = "BRONZE_CATCH_DATA", 
                                           target_table = "SILVER_CATCH_DATA") {
  box::use(duckdb = duckdb[dbConnect])
  box::use(DBI = DBI[dbReadTable, dbExecute, dbExistsTable,dbGetQuery])
  box::use(dplyr = dplyr[mutate, filter, select, coalesce])
  box::use(lubridate = lubridate[dmy, mdy, ymd])
  box::use(logger = logger[log_info, log_error])
  box::use(glue = glue[glue])
  box::use(logger = logger[log_info, log_error])
  
  tryCatch({
    # Read data from the bronze table
    data_bronze <- DBI$dbReadTable(con, source_table)
    data_bronze$ID <- 1:nrow(data_bronze) 
    # Filter unprocessed data if PROCESSING_STATUS column exists
    if ("PROCESSING_STATUS" %in% colnames(data_bronze)) {
      data_bronze <- data_bronze[data_bronze$PROCESSING_STATUS == "UNPROCESSED", ]
    }
    
    if (nrow(data_bronze) == 0) {
      logger$log_info("No unprocessed data found in bronze table.")
      return(0)
    }
    
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
      DBI$dbExecute(con, update_query)
      
      row_count <- nrow(data_silver)
      logger$log_info("Successfully transformed {row_count} rows to silver layer")
      return(row_count)
    } else {
      logger$log_error("Failed to write data to silver table")
      return(0)
    }
    
  }, error = function(e) {
    logger$log_error("Failed to transform data to silver layer: {e$message}")
    return(0)
  })
}
