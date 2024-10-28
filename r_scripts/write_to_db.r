#' Write data to database with error handling
#' @param con database connection
#' @param data processed data frame
#' @return boolean indicating success
write_to_db <- function(con, data) {
  tryCatch({
    dbWriteTable(
      con, 
      name = 'BRONZE_CATCH_DATA', 
      value = data, 
      append = TRUE, 
      row.names = FALSE
    )
    log_info("Successfully wrote {nrow(data)} rows to database")
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Failed to write to database: {e$message}")
    return(FALSE)
  })
}