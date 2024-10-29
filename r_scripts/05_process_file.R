#' Process a single Excel file
#' @param file path to Excel file
#' @param con database connection
#' @return processed data frame or NULL if processing fails
  process_file <- function(file) {
  box::use(readxl = readxl[read_excel])
  
  tryCatch({
    logger::log_info("Processing file: {file}")
    
    # Extract metadata from file path
    path_parts <- stringr::str_split(file, "/")[[1]]
    data_date <- as.Date(path_parts[2])
    file_name <- basename(file)
    country <- stringr::str_extract(file_name, "^[^_]+")
    
    # Read data
    data <- read_excel(file)
    
    # Validate required columns
    required_cols <- c("MONTH", "YEAR", "LICENCE_NO", "HOLDERS_NAME", "LOG_NO", 
                       "PAGE_NO", "DATE", "LOCATION_NAME", "COLLECTION_TIME_HOURS", 
                       "SPECIES", "NUMBER_OF_DEAD_SPECIMENS", "NUMBER_OF_LIVE_SPECIMENS")
    
    missing_cols <- setdiff(required_cols, toupper(names(data)))
    if (length(missing_cols) > 0) {
      logger::log_warn("Missing required columns in {file}: {paste(missing_cols, collapse=', ')}")
    }
    
    # Add metadata columns
    data <- data %>%
      mutate(
        DATA_DATE = data_date,
        COUNTRY = country,
        LOADING_DATETIME = Sys.time(),
        SOURCE_FILE = file_name,
        PROCESSING_STATUS = "LOADED",
        ERROR_MESSAGE = NA_character_
      )
    
    # Standardize column names
    names(data) <- toupper(gsub("[^[:alnum:]]", "_", names(data)))
    
    # Basic data validation
    data <- data %>%
      mutate(
        PROCESSING_STATUS = case_when(
          is.na(DATE) ~ "ERROR_MISSING_DATE",
          is.na(SPECIES) ~ "ERROR_MISSING_SPECIES",
          is.na(LOCATION_NAME) ~ "ERROR_MISSING_LOCATION",
          TRUE ~ PROCESSING_STATUS
        ),
        ERROR_MESSAGE = case_when(
          PROCESSING_STATUS != "LOADED" ~ paste("Missing required data:", PROCESSING_STATUS),
          TRUE ~ ERROR_MESSAGE
        )
      )
    data$ID <- 1:nrow(data)
    
    data <- data |> 
      select(ID,everything())
    
    logger::log_info("Successfully processed file: {file}")
    return(data)
    
  }, error = function(e) {
    logger::log_error("Error processing file {file}: {e$message}")
    return(NULL)
  })
  }
  