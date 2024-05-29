ga_start <- function(ultrasound, ga) {
  checkmate::assert_date(ultrasound, len = 1)
  rutils:::assert_duration(ga)

  ultrasound - ga
}

ga_point <- function(ga_start, point) {
  checkmate::assert_date(ga_start, len = 1)
  checkmate::assert_multi_class(point, c("Date", "POSIXt"))
  rutils:::assert_length_one(point)

  if (point < ga_start) {
    cli::cli_abort(paste(
      "{.strong {cli::col_red('point')}} must be equal or greater than",
      "{.strong {cli::col_blue('ga_start')}}."
    ))
  }

  ga_start <- ga_start |> lubridate::as_date()
  point <- point |> lubridate::as_date()

  out <- lubridate::interval(ga_start, point, tzone = tz) |>
    lubridate::as.duration()

  # `lubridate::dweeks()` gives a time-span of exactly 7 days.
  # nolint start: object_usage_linter.
  weeks <- floor(out / lubridate::dweeks())
  days <- out %% lubridate::dweeks() / lubridate::ddays()
  # nolint end

  cli::cli_alert_info(paste(
    "{.strong",
    "{weeks} {.strong {cli::col_red('week(s)')}}",
    "{days} {.strong {cli::col_red('day(s)')}}",
    "}"
  ))

  invisible(out)
}

ga_week <- function(ga_start, week) {
  checkmate::assert_date(ga_start, len = 1)
  checkmate::assert_number(week, lower = 0)

  week_start <-
    (ga_start + lubridate::dweeks(week)) |>
    lubridate::as_date()

  week_end <-
    (week_start + lubridate::dweeks(1) - lubridate::dseconds(1)) |>
    lubridate::as_date()

  lubridate::interval(week_start, week_end)
}

# ultrasound <- lubridate::dmy("18/10/2022")
# ga <- lubridate::dweeks(6) + lubridate::ddays(3)

# start <- ga_start(ultrasound, ga)
# start

# point <- lubridate::dmy_hms("27/05/2023 05:44:04")
# ga_point(start, point)

# ga_week(start, 37)