#' Create bronze table if it doesn't exist
#' @param con database connection
create_bronze_table <- function(con) {
  box::use(duckdb = duckdb[dbExistsTable])
  box::use(DBI = DBI[dbExecute])
  
  if (!dbExistsTable(con, "BRONZE_CATCH_DATA")) {
    query <- "
    CREATE TABLE BRONZE_CATCH_DATA (
      ID INTEGER,  -- DuckDB will auto-increment
      MONTH INTEGER,
      YEAR INTEGER,
      LICENCE_NO VARCHAR,  -- DuckDB doesn't need length specifications
      HOLDERS_NAME VARCHAR,
      LOG_NO INTEGER,
      PAGE_NO INTEGER,
      NON_COLLECTING_CODE INTEGER,
      DATE VARCHAR,
      LOCATION_NAME VARCHAR,
      COLLECTION_TIME_HOURS DOUBLE,  -- Using DOUBLE instead of NUMERIC for DuckDB
      SPECIES VARCHAR,
      NUMBER_OF_DEAD_SPECIMENS INTEGER,
      NUMBER_OF_LIVE_SPECIMENS INTEGER,
      WEATHER_CONDITION VARCHAR,
      DATA_DATE DATE,
      COUNTRY VARCHAR,
      LOADING_DATETIME TIMESTAMP,
      SOURCE_FILE VARCHAR,
      PROCESSING_STATUS VARCHAR DEFAULT 'UNPROCESSED',
      ERROR_MESSAGE VARCHAR
    );"
    
    dbExecute(con, query)
    logger::log_info("Created BRONZE_CATCH_DATA table")
  }
}