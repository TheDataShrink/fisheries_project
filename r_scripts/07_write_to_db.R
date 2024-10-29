#' Write data to database with error handling and table creation
#' @param con database connection
#' @param data processed data frame
#' @param table_name name of the table to write to
#' @param create_if_missing create table if it doesn't exist
#' @return boolean indicating success
write_to_db <- function(con, data, table_name = 'BRONZE_CATCH_DATA', 
                        create_if_missing = TRUE) {
  box::use(duckdb = duckdb[dbWriteTable, dbExistsTable])
  
  tryCatch({
    # Check if table exists
    if (!dbExistsTable(con, table_name) && create_if_missing) {
      dbWriteTable(
        con, 
        name = table_name, 
        value = data,
        row.names = FALSE
      )
      logger::log_info("Created new table {table_name} with {nrow(data)} rows")
    } else {
      dbWriteTable(
        con, 
        name = table_name, 
        value = data, 
        append = TRUE, 
        row.names = FALSE
      )
      logger::log_info("Appended {nrow(data)} rows to {table_name}")
    }
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Failed to write to database: {e$message}")
    return(FALSE)
  })
}