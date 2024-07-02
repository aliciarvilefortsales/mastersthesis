# !Make a copy of the vault before continuing!

## Anonymize files -----

source(here::here("R", "anonymize_id.R"))

salt <- askpass::askpass() # !Configure `salt` before running!
dir_vault <- normalizePath(readClipboard(), "/", mustWork = FALSE)

anonymize <- function(file, salt) {
  file_dir <- dirname(file)
  file_name <- basename(file)

  if (stringr::str_detect(file_name, "^\\d{11}_")) {
    old_id <- stringr::str_extract(file_name, "^\\d{11}")
    new_id <- anonymize_id(old_id, salt)

    new_file_name <- stringr::str_replace(
      file_name,
      paste0("^", old_id),
      new_id
    )

    new_file <- file.path(file_dir, new_file_name)

    file.rename(file, new_file)

    new_file
  } else {
    file
  }
}

vapply(
  X = list.files(dir_vault, full.names = TRUE, recursive = TRUE),
  FUN = anonymize,
  FUN.VALUE = character(1),
  salt = salt
)

## Settings -----

password <- askpass::askpass()

dir_bundles <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_processed_data <- normalizePath(readClipboard(), "/", mustWork = FALSE)
dir_raw_data <- normalizePath(readClipboard(), "/", mustWork = FALSE)

dir_raw_actigraphy <- file.path(dir_raw_data, "actigraphy")
dir_raw_consent <- file.path(dir_raw_data, "consent")
dir_raw_control_form <- file.path(dir_raw_data, "control-form")
dir_raw_delivery_receipt <- file.path(dir_raw_data, "delivery-receipt")
dir_raw_field_form <- file.path(dir_raw_data, "field-form")
dir_raw_medical_record <- file.path(dir_raw_data, "medical-record")
dir_raw_pilot_form <- file.path(dir_raw_data, "pilot-form")
dir_raw_pregnancy_booklet <- file.path(dir_raw_data, "pregnancy-booklet")
dir_raw_return_receipt <- file.path(dir_raw_data, "return-receipt")
dir_raw_sleep_diary <- file.path(dir_raw_data, "sleep-diary")

dir_processed_actigraphy <- file.path(dir_processed_data, "actigraphy")
dir_processed_sleep_diary <- file.path(dir_processed_data, "sleep-diary")

file_field_form <- file.path(dir_raw_field_form, "raw.csv")
file_pilot_form <- file.path(dir_raw_pilot_form, "raw.csv")
file_sleep_diary <- file.path(dir_raw_sleep_diary, "raw.csv")

## Get CPF & IDS -----

source(here::here("R", "anonymize_id.R"))
source(here::here("R", "test_cpf.R"))

# {actschool}: cpf_col_index <- 3
# {pregnancy}: cpf_col_index <- 5

raw_data_field_form <-
  file_field_form |>
  readr::read_csv(
    col_types = readr::cols(.default = "c"),
    na = c("", "NA")
    ) |>
  rutils:::shush()

raw_data_pilot_form <-
  file_pilot_form |>
  readr::read_csv(
    col_types = readr::cols(.default = "c"),
    na = c("", "NA")
  ) |>
  rutils:::shush()

cpf_values <- c(raw_data_field_form[[5]], raw_data_pilot_form[[5]])

id_data <- dplyr::tibble(
  id = anonymize_id(cpf_values, salt = salt),
  cpf = cpf_values
)

test_cpf(id_data$cpf)

## Load, filter & write data -----

for (i in split(id_data, seq(nrow(id_data)))) {
  raw_data_field_form <-
    file_field_form |>
    readr::read_csv(
      col_types = readr::cols(.default = "c"),
      na = c("", "NA"),
    ) |>
    scaler:::filter_data(col_index = 5, value = i$cpf) |>
    rutils:::shush()

  # raw_data_field_form <-
  #   raw_data_field_form %>% # Don't change the pipe
  #   dplyr::filter(lubridate::year(lubridate::dmy_hms(.[[1]])) == "2022")

  raw_data_pilot_form <-
    file_pilot_form |>
    readr::read_csv(
      col_types = readr::cols(.default = "c"),
      na = c("", "NA"),
    ) |>
    scaler:::filter_data(col_index = 5, value = i$cpf) |>
    rutils:::shush()

  if (nrow(raw_data_field_form) == 0) {
    raw_data_field_form <- raw_data_pilot_form
  }

  raw_data_sleep_diary <-
    file_sleep_diary |>
    readr::read_csv(
      col_types = readr::cols(.default = "c"),
      na = c("", "NA"),
    ) |>
    scaler:::filter_data(col_index = 3, value = i$cpf)

  # raw_data_sleep_diary <-
  #   raw_data_sleep_diary %>% # Don't change the pipe
  #   dplyr::filter(lubridate::year(lubridate::dmy_hms(.[[1]])) == "2022")

  if (!nrow(raw_data_sleep_diary) == 0) {
    # {actschool}: col_indexes = c(1, 4, 8, 10)
    # {pregnancy}: col_indexes = c(1, 4, 8, 10)
    sleep_diary_type_of_day <-
      raw_data_sleep_diary |>
      scaler:::get_sleep_diary_type_of_day(col_indexes = c(1, 4, 8, 10))

    # {actschool}: col_indexes = c(1, 8, 10, 17:26)
    # {pregnancy}: col_indexes = c(1, 8, 10, 19:28)
    tidy_data_sleep_diary <-
      raw_data_sleep_diary |>
      scaler:::tidy_sleep_diary(col_indexes = c(1, 8, 10, 19:28))
  }

  if (!nrow(raw_data_field_form) == 0) {
    raw_data_field_form |>
      readr::write_csv(
        file.path(dir_raw_field_form, paste0(i$id, "_field-form", ".csv"))
      )
  }

  if (!nrow(raw_data_sleep_diary) == 0) {
    raw_data_sleep_diary |>
      readr::write_csv(
        file.path(dir_raw_sleep_diary, paste0(i$id, "_sleep-diary", ".csv"))
      )

    sleep_diary_type_of_day |>
      readr::write_csv(
        file.path(
          dir_processed_sleep_diary,
          paste0(i$id, "_sleep-diary-type-of-day", ".csv")
        )
      )

    tidy_data_sleep_diary |>
      scaler:::actstudio_sleep_diary(
        file = file.path(
          dir_processed_actigraphy,
          paste0(i$id, "_actigraphy-sleep-diary", ".txt")
        )
      )
  }
}

## Create bundles -----

for (i in split(id_data, seq(nrow(id_data)))) {
  bundle_files <- c(
    "actigraphy-raw-data" = file.path(
      dir_raw_actigraphy, paste0(i$id, "_actigraphy-raw-data", ".txt")
    ),
    "actigraphy-raw-data_report" = file.path(
      dir_raw_actigraphy, paste0(i$id, "_actigraphy-raw-data-report", ".txt")
    ),
    "actigraphy-processed-data" = file.path(
      dir_processed_actigraphy, paste0(i$id, "_actigraphy-processed-data", ".txt")
    ),
    "actigraphy-sleep-diary" = file.path(
      dir_processed_actigraphy, paste0(i$id, "_actigraphy-sleep-diary", ".txt")
    ),
    "consent" = file.path(
      dir_raw_consent, paste0(i$id, "_consent", ".pdf")
    ),
    "delivery-receipt" = file.path(
      dir_raw_delivery_receipt, paste0(i$id, "_delivery-receipt", ".pdf")
    ),
    "field-form" = file.path(
      dir_raw_field_form, paste0(i$id, "_field-form", ".csv")
    ),
    "medical-record" = file.path(
      dir_raw_medical_record, paste0(i$id, "_medical-record", ".pdf")
    ),
    "pregnancy-booklet" = file.path(
      dir_raw_pregnancy_booklet, paste0(i$id, "_pregnancy-booklet", ".pdf")
    ),
    "return-receipt" = file.path(
      dir_raw_return_receipt, paste0(i$id, "_return-receipt", ".pdf")
    ),
    "sleep-diary" = file.path(
      dir_raw_sleep_diary, paste0(i$id, "_sleep-diary", ".csv")
    ),
    "sleep-diary-type-of-day" = file.path(
      dir_processed_sleep_diary, paste0(i$id, "_sleep-diary-type-of-day", ".csv")
    )
  )

  for (j in bundle_files) {
    if (!file.exists(j)) {
      bundle_files <- bundle_files[!bundle_files %in% j]
    }
  }

  if (!length(bundle_files) == 0) {
    utils::zip(
      zipfile = file.path(dir_bundles, i$id),
      files = bundle_files,
      flags = paste("--password", password),
      extras = "-j"
    )
  }
}

## Lock files -----

dir_path <- normalizePath(readClipboard(), "/", mustWork = FALSE)
private_key <- rstudioapi::selectFile()
public_key <- rstudioapi::selectFile()
password <- askpass::askpass()

lockr::lock_dir(
  dir = dir_path,
  public_key = public_key,
  suffix = ".lockr",
  remove_file = TRUE
)

# test_file <- rstudioapi::selectFile()
#
# lockr::unlock_file(
#   file = test_file,
#   private_key = private_key,
#   suffix = ".lockr",
#   remove_file = FALSE,
#   password = password
# )

## Unlock files -----

dir_path <- normalizePath(readClipboard(), "/", mustWork = FALSE)
private_key <- rstudioapi::selectFile()
public_key <- rstudioapi::selectFile()
password <- askpass::askpass()

lockr::unlock_dir(
  dir = dir_path,
  private_key  = private_key ,
  suffix = ".lockr",
  remove_file = TRUE,
  password = password
)

# Get metadata (remove) -----

form_date <-
  raw_data_field_form[[1, 1]] |>
  lubridate::dmy_hms() |>
  lubridate::date()

# {actschool}: birth_date_col_index <- 81
birth_date <-
  raw_data_field_form[[1, 4]] |>
  lubridate::dmy()

age <-
  lubridate::interval(birth_date, form_date, tz = "America/Sao_Paulo") |>
  lubridate::as.period() |>
  lubridate::year()

# {actschool}: sex_col_index <- 84
# sex <- ifelse(raw_data_field_form[[1, 84]] == "Masculino", "Male", "Female")
# weight <- as.numeric(raw_data_field_form[[1, 77]])
# height <- as.numeric(raw_data_field_form[[1, 78]]) / 100
# bmi_cat <- scaler:::bmi(weight, height, number = FALSE, cat = TRUE)

# {actschool}: dominant_wrist_col_index <- 79
dominant_wrist <- ifelse(
  raw_data_field_form[[1, 98]] == "Esquerdo", "Left", "Right"
  )

non_dominant_wrist <- ifelse(dominant_wrist == "Left", "Right", "Left")

start <- lubridate::dmy_hms("19/12/2022 17:36:00")
end <- lubridate::dmy_hms("20/01/2023 11:35:00")

days <-
  (end - start) |>
  as.numeric() |>
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
