#' Establish database connection with error handling
#' @return database connection object or NULL if connection fails
connect_to_db <- function() {
  box::use(duckdb = duckdb[duckdb,dbConnect])
  
  tryCatch({
    con <- dbConnect(
      duckdb::duckdb(),
      dbname = 'fisheries_db',
      user = 'duckdb',
      password = 'duckdb'
    )
    logger::log_info("Successfully connected to database")
    return(con)
  }, error = function(e) {
    logger::log_error("Failed to connect to database: {e$message}")
    return(NULL)
  })
}

