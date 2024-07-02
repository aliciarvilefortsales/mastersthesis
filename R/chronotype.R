# library(checkmate, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(mctq, quietly = TRUE)
# library(rutils, quietly = TRUE)

# wd <- 6 # Número total de dias de trabalho
# fd <- 17 # Número total de dias livres
# # FPS nos dias de trabalho
# sd_w <- lubridate::as.duration(hms::parse_hms("07:01:10"))
# so_f <- hms::parse_hms("01:55:42") # HIS nos dias livres
# # FPS nos dias livres
# sd_f <- lubridate::as.duration(hms::parse_hms("07:24:31"))
#
# chronotype(wd, fd, so_f, sd_w, sd_f)

#' Compute the MCTQ chronotype based on actigraphy data
#'
#' @description
#'
#' This function computes the MCTQ
#' __sleep-corrected local time of mid-sleep on work-free days__
#' (chronotype) based on mean values extracted from actigraphy data.
#'
#' @details
#'
#' If the subject doesn't have any workdays, set `wd` and `sd_w` to `NULL`. In
#' this case the chronotype will be the
#' __local time of mid-sleep on work-free day__ without any correction.
#'
#' If the subject doesn't have any work-free days, it's impossible to compute
#' the chronotype, since we don't have any information about his/her
#' unrestrained sleep.
#'
#' Please note that this function assumes that the
#' __alarm clock use on work-free days__ is always set to `FALSE`.
#'
#' Learn more about this computation by consulting the `mctq` R package
#' documentation. You can find it
#' [here](https://docs.ropensci.org/mctq/reference/msf_sc.html).
#'
#' @param wd An [integerish][checkmate::test_integerish()]
#'   [`numeric`][base::numeric()] object or an [`integer`][base::integer()]
#'   object corresponding to the __number of workdays__ present in the
#'   actigraphy data. If the subject doesn't have any workdays, set it to
#'   `NULL` (default: `NULL`).
#' @param fd An [integerish][checkmate::test_integerish()]
#'  [`numeric`][base::numeric()] object or an [`integer`][base::integer()]
#'  object corresponding to the __number of work-free days__ present in the
#'  actigraphy data.
#' @param so_f An [`hms`][hms::hms()] object corresponding to the __mean__ of
#'   the __local time of sleep onset on work-free days__ of the actigraphy data.
#' @param sd_w A [`Duration`][lubridate::duration()] object corresponding to the
#'   __mean__ of the __sleep duration on work days__ of the actigraphy data. If
#'   the subject doesn't have any workdays, set it to `NULL` (default: `NULL`).
#' @param sd_f A [`Duration`][lubridate::duration()] object corresponding to the
#'   __mean__ of the __sleep duration on work-free days__ of the actigraphy
#'   data.
#' @param round A [`logical`][base::logical()] flag indicating whether to round
#'   the result to the nearest second (default: `TRUE`).
#'
#' @return An [`hms`][hms::hms()] object corresponding to the MCTQ
#'   __sleep-corrected local time of mid-sleep on work-free days__ (chronotype).
#'
#' @examples
#' chronotype(
#'   wd = 6,
#'   fd = 17,
#'   sd_w = lubridate::as.duration(hms::parse_hms("07:01:10")),
#'   so_f = hms::parse_hms("01:55:42"),
#'   sd_f = lubridate::as.duration(hms::parse_hms("07:24:31"))
#' )
#' #> 05:34:55 # Expected
chronotype <- function(wd = NULL, fd, so_f, sd_w = NULL, sd_f, round = TRUE) {
  checkmate::assert_integerish(wd, lower = 0, null.ok = TRUE)
  checkmate::assert_integerish(fd, lower = 0)
  rutils:::assert_hms(so_f, lower = hms::hms(0))
  rutils:::assert_duration(sd_w, lower = lubridate::duration(0), null.ok = TRUE)
  rutils:::assert_duration(sd_f, lower = lubridate::duration(0))

  msf <- mctq::msl(so_f, sd_f)

  if (is.null(wd) || is.null(sd_w)) {
    rutils:::assert_identical(fd, so_f, sd_f, type = "length")

    msf_sc <- msf
  } else {
    rutils:::assert_identical(wd, fd, sd_w, so_f, sd_f, type = "length")

    sd_mean <- ((sd_w * wd) + (sd_f * fd)) / (wd + fd)
    msf_sc <- mctq::msf_sc(msf, sd_w, sd_f, sd_mean, FALSE)
  }

  if (isTRUE(round)) {
    lubritime::round_time(msf_sc)
  } else {
    msf_sc
  }
}
