# Copy this code to another file when using it!

# Settings -----

# "0000-0000-0000-0000" (ORCID) or CPF "00000000000"
filter_value <- ""

# Insert the project password here.
password <- ""

dir_actigraphy <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_field_form <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_sleep_diary <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_control <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_records <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_bundles <- normalizePath(readClipboard(), "/", mustWork = FALSE)
# dir_img <- normalizePath(readClipboard(), "/", mustWork = FALSE)

actstudio_sleep_diary_file_name <- paste0(
  filter_value, "_actigraphy-sleep-diary", ".txt"
)
field_form_file_name <- paste0(
  filter_value, "_field-form", ".csv"
)
sleep_diary_file_name <- paste0(
  filter_value, "_sleep-diary", ".csv"
)
sleep_diary_type_of_day_file_name <- paste0(
  filter_value, "_sleep-diary-type-of-day", ".csv"
)

# start <- lubridate::dmy_hms("19/12/2022 17:36:00")
# end <- lubridate::dmy_hms("20/01/2023 11:35:00")

file_field_form <- file.path(dir_field_form, "raw.csv")
file_sleep_diary <- file.path(dir_sleep_diary, "raw.csv")

# Load & filter data -----

# {actschool}: col_index = 3
# {pregnancy.c}: col_index = 5
col_index <- 5

raw_data_field_form <-
  file_field_form |>
  readr::read_csv(na = c("", "NA"), col_types = readr::cols(.default = "c")) |>
  scaler:::filter_data(col_index = col_index, value = filter_value) |>
  rutils:::shush()

# raw_data_field_form <-
#   raw_data_field_form %>% # Don't change the pipe
#   dplyr::filter(lubridate::year(lubridate::dmy_hms(.[[1]])) == "2022")

# {actschool}: col_index = 3
# {pregnancy.c}: col_index = 3
raw_data_sleep_diary <-
  file_sleep_diary |>
  readr::read_csv(na = c("", "NA"), col_types = readr::cols(.default = "c")) |>
  scaler:::filter_data(col_index = 3, value = filter_value)

# raw_data_sleep_diary <-
#   raw_data_sleep_diary %>% # Don't change the pipe
#   dplyr::filter(lubridate::year(lubridate::dmy_hms(.[[1]])) == "2022")


# Process data -----

if (!nrow(raw_data_sleep_diary) == 0) {
  # {actschool}: col_indexes = c(1, 4, 8, 10)
  # {pregnancy.c}: col_indexes = c(1, 4, 8, 10)
  sleep_diary_type_of_day <-
    raw_data_sleep_diary |>
    scaler:::get_sleep_diary_type_of_day(col_indexes = c(1, 4, 8, 10))

  # {actschool}: col_indexes = c(1, 8, 10, 17:26)
  # {pregnancy.c}: col_indexes = c(1, 8, 10, 19:28)
  tidy_data_sleep_diary <-
    raw_data_sleep_diary |>
    scaler:::tidy_sleep_diary(col_indexes = c(1, 8, 10, 19:28))
}


# Write data -----

# ActStudio sleep diary
tidy_data_sleep_diary |>
  scaler:::actstudio_sleep_diary(
    file = file.path(dir_actigraphy, actstudio_sleep_diary_file_name)
  )

# Sleep diary data
raw_data_sleep_diary |>
  readr::write_csv(file.path(dir_sleep_diary, sleep_diary_file_name))

# Sleep diary type of day data
sleep_diary_type_of_day |>
  readr::write_csv(
    file.path(dir_sleep_diary, sleep_diary_type_of_day_file_name)
  )

# Field form data
raw_data_field_form |>
  readr::write_csv(file.path(dir_field_form, field_form_file_name))


# Create bundle -----

bundle_files <- c(
  "consent" = file.path(
    dir_control, paste0(filter_value, "_consent", ".pdf")
  ),
  "delivery-receipt" = file.path(
    dir_control, paste0(filter_value, "_delivery-receipt", ".pdf")
  ),
  "return-receipt" = file.path(
    dir_control, paste0(filter_value, "_return-receipt", ".pdf")
  ),
  "actigraphy-raw-data" = file.path(
    dir_actigraphy, paste0(filter_value, "_actigraphy-raw-data", ".txt")
  ),
  "actigraphy-raw-data_report" = file.path(
    dir_actigraphy, paste0(filter_value, "_actigraphy-raw-data-report", ".txt")
  ),
  "actigraphy-sleep-diary" = file.path(
    dir_actigraphy, paste0(filter_value, "_actigraphy-sleep-diary", ".txt")
  ),
  "field-form" = file.path(
    dir_field_form, paste0(filter_value, "_field-form", ".csv")
  ),
  "sleep-diary" = file.path(
    dir_sleep_diary, paste0(filter_value, "_sleep-diary", ".csv")
  ),
  "sleep-diary-type-of-day" = file.path(
    dir_sleep_diary, paste0(filter_value, "_sleep-diary-type-of-day", ".csv")
  ),
  "pregnancy-booklet" = file.path(
    dir_records, paste0(filter_value, "_pregnancy-booklet", ".pdf")
  ),
  "medical-record" = file.path(
    dir_records, paste0(filter_value, "_medical-record", ".pdf")
  )
)

utils::zip(
  zipfile = file.path(dir_bundles, filter_value),
  files = bundle_files,
  flags = paste("--password", password),
  extras = "-j"
)


# Get metadata -----

form_date <-
  raw_data_field_form[[1, 1]] |>
  lubridate::dmy_hms() |>
  lubridate::date()

birth_date <-
  raw_data_field_form[[1, 81]] |>
  lubridate::dmy()

age <-
  lubridate::interval(birth_date, form_date, tz = "America/Sao_Paulo") |>
  lubridate::as.period() |>
  lubridate::year()

sex <- ifelse(raw_data_field_form[[1, 84]] == "Masculino", "Male", "Female")
weight <- as.numeric(raw_data_field_form[[1, 77]])
height <- as.numeric(raw_data_field_form[[1, 78]]) / 100
bmi_cat <- scaler:::bmi(weight, height, number = FALSE, cat = TRUE)

dominant_wrist <- ifelse(
  raw_data_field_form[[1, 79]] == "Esquerdo", "Left", "Right"
  )

non_dominant_wrist <- ifelse(dominant_wrist == "Left", "Right", "Left")

days <-
  (end - start) |>
  as.numeric |>
  floor()

cli::cli_bullets(c(
  "*" = "Age: {age}",
  "*" = "Sex: {sex}",
  "*" = "Height: {height} m",
  "*" = "Weight: {weight} kg",
  "*" = "BMI: {bmi_cat}",
  "*" = "Body part: {non_dominant_wrist} wrist (non-dominant wrist)",
  "*" = "Start: {start}",
  "*" = "End: {end}",
  "*" = "Days: {days}"
))
