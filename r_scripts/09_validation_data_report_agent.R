library(pointblank)

# Create an agent
agent <- create_agent(tbl = bronze_tbl, tbl_name = "Bronze Layer Data")

# Define validation steps
agent <- agent %>%
  col_is_numeric(columns = vars(CATCH_DATA_UID, COLLECTION_TIME_HOURS, NUMBER_OF_DEAD_SPECIMENS, NUMBER_OF_LIVE_SPECIMENS)) %>%
  col_is_character(columns = vars(VESSEL_NAME, LICENSE_NUMBER, COUNTRY, SPECIES, LOCATION_NAME)) %>%
  col_is_date(columns = vars(CATCH_DATE)) %>%
  col_vals_between(
    columns = vars(CATCH_DATE),
    left = as.Date('2010-01-01'),
    right = Sys.Date(),
    na_pass = FALSE
  ) %>%
  col_vals_gte(columns = vars(COLLECTION_TIME_HOURS), value = 0) %>%
  col_vals_lte(columns = vars(COLLECTION_TIME_HOURS), value = 24) %>%
  col_vals_gte(columns = vars(NUMBER_OF_DEAD_SPECIMENS, NUMBER_OF_LIVE_SPECIMENS), value = 0) %>%
  col_vals_not_null(columns = vars(CATCH_DATA_UID, VESSEL_NAME, CATCH_DATE, SPECIES)) %>%
  col_vals_not_null(columns = vars(CATCH_DATA_UID))

# Interrogate the data
agent <- agent %>% interrogate()
fs::dir_create("validation_reports")
# Generate a report
report_file <- paste0("validation_reports/validation_report_", Sys.Date(), ".html")
agent %>% export_report(filename = report_file)

# Optionally, view the report in RStudio Viewer
# agent %>% get_agent_report()

# Handle validation failures
if (all_passed(agent)) {
  message("All data validation checks passed.")
  # Proceed with further processing
} else {
  warning("Data validation checks failed. Check the report for details.")
  # Optionally, stop the pipeline or handle the issues
  # stop("Data validation failed.")
}

