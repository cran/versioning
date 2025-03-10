## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# install.packages('versioning')
library(versioning)

## ----show-config--------------------------------------------------------------
example_config_fp <- system.file('extdata', 'example_config.yaml', package = 'versioning')

# Print the contents of the input YAML file
file_contents <- system(paste('cat', example_config_fp), intern = T)
message(paste(file_contents, collapse ='\n'))

## ----load-config--------------------------------------------------------------
# Load YAML file as a Config object
config <- versioning::Config$new(config_list = example_config_fp)

# Print the config file contents
print(config)

## ----retrieve-settings--------------------------------------------------------
# Retrieve some example settings from the config file
message("config$get('a') yields: ", config$get('a'))
message("config$get('b') yields: ", config$get('b'))
message("config$get('group_c', 'd') yields: ", config$get('group_c', 'd'))

# Update a setting
config$config_list$a <- 12345
message("config$get('a') has been updated and now yields: ", config$get('a'))

## ----get-directories----------------------------------------------------------
# Update the raw_data and prepared_data directories to temporary directories for this
# example
config$config_list$directories$raw_data$path <- tempdir(check = T)
config$config_list$directories$prepared_data$path <- tempdir(check = T)

# Create directories
message(
  "Creating raw_data directory, which is not versioned: ",
  config$get_dir_path('raw_data')
)
dir.create(config$get_dir_path('raw_data'), showWarnings = FALSE)

message(
  "Creating prepared_data directory, which is versioned: ",
  config$get_dir_path('prepared_data')
)
dir.create(config$get_dir_path('prepared_data'), showWarnings = FALSE)

# Copy the example input file to the raw data folder
file.copy(
  from = system.file('extdata', 'example_input_file.csv', package = 'versioning'),
  to = config$get_file_path(dir_name = 'raw_data', file_name = 'a')
)

## ----read-write-files---------------------------------------------------------
# Read that same table from file
df <- config$read(dir_name = 'raw_data', file_name = 'a')

# Write a prepared table and a summary to file
config$write(df, dir_name = 'prepared_data', file_name = 'prepared_table')
config$write(
  paste("The prepared table has", nrow(df), "rows and", ncol(df), "columns."),
  dir_name = 'prepared_data',
  file_name = 'summary_text'
)

# Both files should now appear in the "prepared_data" directory
list.files(config$get_dir_path('prepared_data')) 

## ----get-supported-extensions-------------------------------------------------
message(
  "Supported file types for reading: ",
  paste(sort(names(versioning::get_file_reading_functions())), collapse = ', ')
)
message(
  "Supported file types for writing: ",
  paste(sort(names(versioning::get_file_writing_functions())), collapse = ', ')
)

## ----write-self---------------------------------------------------------------
# Write the config object to the "prepared_data" directory
config$write_self(dir_name = 'prepared_data')
# The "prepared_data" directory should now include "config.yaml"
list.files(config$get_dir_path('prepared_data'))

## ----update-versions----------------------------------------------------------
# Load a new custom config where the "prepared_data" version has been updated to "v2"
custom_versions <- list(prepared_data = 'v2')
config_v2 <- versioning::Config$new(
  config_list = example_config_fp,
  versions = custom_versions
)
print(config_v2$get_dir_path('prepared_data')) # Should now end in ".../v2"

