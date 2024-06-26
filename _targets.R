# See <https://books.ropensci.org/targets/> to learn more.

# library(here)
library(tarchetypes)
library(targets)

targets::tar_option_set(
  packages = c(
    "lubridate", # For masking reasons.
    "checkmate", "cli", "curl", "dplyr", "here", "hms", "lockr", "lubritime",
    "mctq", "readr", "rlang", "rutils", "scaler", "stringr", "tidyr", "utils"
    )
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
# targets::tar_source(files = here::here("R"))
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  targets::tar_target(name = raw_data, command = get_raw_data()),
  targets::tar_target(name = tidy_data, command = tidy_data_(raw_data)),
  targets::tar_target(name = validated_data,
                      command = validate_data(tidy_data)),
  targets::tar_target(name = analyzed_data,
                      command = analyze_data(validated_data)),
  targets::tar_target(name = filtered_data,
                      command = filter_data(analyzed_data))
  # tarchetypes::tar_quarto(name = book, path = here::here())
)
