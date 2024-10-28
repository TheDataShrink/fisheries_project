#' Establish database connection with error handling
#' @return database connection object or NULL if connection fails
connect_to_db <- function() {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = 'fisheries_db',
      host = 'db',
      port = 5432,
      user = 'postgres',
      password = 'postgres'
    )
    logger::log_info("Successfully connected to database")
    return(con)
  }, error = function(e) {
    logger::log_error("Failed to connect to database: {e$message}")
    return(NULL)
  })
}