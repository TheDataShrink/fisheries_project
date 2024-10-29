#' Initialize control table in database
#' @param con DuckDB connection
#' @return boolean indicating success
initialize_control_table <- function(con) {
# Load required libraries
box::use(duckdb = duckdb[dbConnect])
box::use(DBI = DBI[dbExecute, dbGetQuery])
box::use(logger = logger[log_info, log_error])
box::use(lubridate = lubridate[now])
  tryCatch({
    query <- "
    CREATE TABLE PROCESS_CONTROL (
  PROCESS_CONTROL_UID VARCHAR NOT NULL,  -- Store UUID as VARCHAR
  FILE_NAME VARCHAR NOT NULL,
  FILE_PATH VARCHAR NOT NULL,
  RECEIVED_DATETIME TIMESTAMP NOT NULL,
  PROCESSED_DATETIME TIMESTAMP,
  STATUS VARCHAR NOT NULL, -- 'RECEIVED', 'PROCESSING', 'SUCCESS', 'FAILED'
  STEP VARCHAR,            -- Current processing step
  ERROR_MESSAGE VARCHAR,
  ROW_COUNT INTEGER,
  DATA_DATE DATE,
  PRIMARY KEY (PROCESS_CONTROL_UID)
);"
    dbExecute(con, query)
    log_info("Successfully initialized control table")
    return(TRUE)
  }, error = function(e) {
    log_error("Failed to initialize control table: {e$message}")
    message("PROCESS_CONTROL table already exists")
    return(FALSE)
  })
}
