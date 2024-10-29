#' Establish persistent database connection with error handling
#' @param db_path Path where the database file should be stored
#' @return database connection object or NULL if connection fails
connect_to_db <- function(db_path = "data/fisheries.duckdb") {
  box::use(duckdb = duckdb[duckdb, dbConnect])
  
  # Create directory if it doesn't exist
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  
  tryCatch({
    con <- dbConnect(
      duckdb::duckdb(),
      dbdir = db_path,
      read_only = FALSE
    )
    logger::log_info("Successfully connected to database at {db_path}")
    return(con)
  }, error = function(e) {
    logger::log_error("Failed to connect to database: {e$message}")
    return(NULL)
  })
}