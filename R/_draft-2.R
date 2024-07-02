# Others ---

days <-
  (end - start) |>
  as.numeric() |>
  round()

days

ultrasound <- lubridate::dmy("18/10/2022")
ga <- lubridate::dweeks(6) + lubridate::ddays(3)

start <- ga_start(ultrasound, ga)
start

point <- lubridate::dmy_hms("27/05/2023 05:44:04")
ga_point(start, point)

ga_week(start, 37)
