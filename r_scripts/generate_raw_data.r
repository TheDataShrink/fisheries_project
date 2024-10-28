# r_scripts/generate_raw_data.R

## Load necessary libraries
library(dplyr)
library(lubridate)
library(openxlsx)
library(stringr)
library(tidyr)
library(fs)

# Set seed for reproducibility
set.seed(123)

# Define possible values
months <- month.name
years <- 2019:2024
licence_numbers <- sprintf("LN%04d", 1:50)
holder_names <- c("John Doe", "Jane Smith", "Alice Johnson", "Bob Lee", "Maria Garcia")
locations <- c("Beach A", "Reef B", "Beach C", "Reef D", "Beach E")
species_list <- c("Conch", "Scallop", "Oyster", "Clam", "Mussel")
non_collecting_codes <- c(1, 5)  # 1 = weather, 5 = other
countries <- c("Japan", "Korea", "Taiwan")

# Define the number of records per file
num_records <- 50

# Define dates for data files
data_dates <- seq.Date(from = as.Date("2024-10-01"), by = "day", length.out = 5)

# Create directories for raw data if they don't exist
dir_create("raw_data")
purrr::walk(data_dates, ~dir_create(path = file.path("raw_data", as.character(.x))))

# Function to generate data for one file
generate_data_file <- function(country, date) {
  # Convert date to proper Date object if it isn't already
  date <- as.Date(date)
  
  # Generate header information
  header_info <- tibble(
    MONTH = month(date),
    YEAR = year(date),
    LICENCE_NO = sample(licence_numbers, 1),
    HOLDERS_NAME = sample(holder_names, 1),
    LOG_NO = sample(600:700, 1),
    PAGE_NO = sample(1:10, 1),
    NON_COLLECTING_CODE = sample(c(non_collecting_codes, NA), 1, prob = c(0.1, 0.1, 0.8))
  )
  
  # Generate collection data
  collection_data <- tibble(
    DATE = date + sample(0:30, num_records, replace = TRUE),  # Generate actual dates
    LOCATION_NAME = sample(locations, num_records, replace = TRUE),
    COLLECTION_TIME_HOURS = round(runif(num_records, min = 0.5, max = 8), 2),
    SPECIES = sample(species_list, num_records, replace = TRUE),
    NUMBER_OF_DEAD_SPECIMENS = sample(0:20, num_records, replace = TRUE),
    NUMBER_OF_LIVE_SPECIMENS = sample(0:50, num_records, replace = TRUE)
  )
  
  # Introduce data inconsistencies (but in a controlled way)
  collection_data <- collection_data %>%
    mutate(
      DATE = case_when(
        row_number() %% 3 == 0 ~ format(DATE, "%d-%b-%Y"),
        row_number() %% 3 == 1 ~ format(DATE, "%m/%d/%Y"),
        TRUE ~ format(DATE, "%Y-%m-%d")
      )
    )
  
  # Inconsistent data types for LICENCE_NO (only for some countries)
  if (country == "Korea" && runif(1) < 0.2) {
    header_info$LICENCE_NO <- as.numeric(sub("LN", "", header_info$LICENCE_NO))
  }
  
  # Introduce missing values
  collection_data <- collection_data %>%
    mutate(
      NUMBER_OF_LIVE_SPECIMENS = if_else(row_number() %in% sample(1:num_records, 5), NA_real_, NUMBER_OF_LIVE_SPECIMENS),
      COLLECTION_TIME_HOURS = if_else(row_number() %in% sample(1:num_records, 3), NA_real_, COLLECTION_TIME_HOURS)
    )
  
  # Country-specific modifications
  final_data <- if (country == "Japan") {
    # Add weather conditions for Japan
    collection_data %>%
      mutate(WEATHER_CONDITION = sample(c("Sunny", "Rainy", "Cloudy"), num_records, replace = TRUE))
  } else if (country == "Korea") {
    # Remove non-collecting code for Korea
    header_info$NON_COLLECTING_CODE <- NULL
    collection_data
  } else {
    # Default case (Taiwan)
    collection_data
  }
  
  # Combine header and collection data
  data_to_save <- bind_cols(
    header_info %>% slice(rep(1, num_records)),  # Repeat header info for each row
    final_data
  )
  
  # Create file path
  file_name <- paste0(country, "_Marine_Shell_Logbook_", format(date, "%Y-%m-%d"), ".xlsx")
  file_path <- file.path("raw_data", format(date, "%Y-%m-%d"), file_name)
  
  # Save to Excel file
  write.xlsx(data_to_save, file = file_path, overwrite = TRUE)
  
  # Return the path for confirmation
  return(file_path)
}

# Generate data files for each country and date
generated_files <- crossing(date = data_dates, country = countries) %>%
  purrr::pmap_chr(~generate_data_file(country = .y, date = .x))

# Print confirmation of generated files
cat("Generated", length(generated_files), "files:\n")
cat(paste("-", generated_files), sep = "\n")
