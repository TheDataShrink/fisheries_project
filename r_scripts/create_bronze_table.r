#' Create bronze table if it doesn't exist
#' @param con database connection
create_bronze_table <- function(con) {
  if (!dbExistsTable(con, "BRONZE_CATCH_DATA")) {
    query <- "
    CREATE TABLE BRONZE_CATCH_DATA (
      ID SERIAL PRIMARY KEY,
      MONTH INTEGER,
      YEAR INTEGER,
      LICENCE_NO VARCHAR(10),
      HOLDERS_NAME VARCHAR(100),
      LOG_NO INTEGER,
      PAGE_NO INTEGER,
      NON_COLLECTING_CODE INTEGER,
      DATE VARCHAR(20),
      LOCATION_NAME VARCHAR(100),
      COLLECTION_TIME_HOURS NUMERIC,
      SPECIES VARCHAR(50),
      NUMBER_OF_DEAD_SPECIMENS INTEGER,
      NUMBER_OF_LIVE_SPECIMENS INTEGER,
      WEATHER_CONDITION VARCHAR(50),
      DATA_DATE DATE,
      COUNTRY VARCHAR(50),
      LOADING_DATETIME TIMESTAMP,
      SOURCE_FILE VARCHAR(255),
      PROCESSING_STATUS VARCHAR(20) DEFAULT 'UNPROCESSED',
      ERROR_MESSAGE TEXT
    )"
    dbExecute(con, query)
    logger::log_info("Created BRONZE_CATCH_DATA table")
  }
}