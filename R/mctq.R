# # Notes
#
# * Source the file before running the functions.
# * Don't forget to uncomment the 'library' functions below.

# library(checkmate)
# library(cli)
# library(dplyr)
# library(googlesheets4)
# library(hms)
# library(lubridate)
# library(mctq)
# library(rlang)
# library(utils)
# library(validate)


#' Read the standard MCTQ raw dataset from Google Sheets
#'
#' @description
#'
#' `read_std_mctq()` reads a raw dataset of composed of basic/measurable
#' variables of the Munich ChronoType Questionnaire (MCTQ) standard version
#' from a Google Spreadsheet.
#'
#' The data must conform to the "Raw data" sheet structure from the following
#' Google Spreadsheet: https://bit.ly/3wwWnIt .
#'
#' @param id (optional) a string indicating the Google Spreadsheet ID (default:
#'   `"1dgbg96BpA9jS0O7dt8onGzHodgefNut7RAc4IwBvqJ8"`).
#' @param sheet (optional) a string indicating the sheet name where the raw data
#'   is stored (default: `"Raw data"`.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a raw standard MCTQ
#'   dataset.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(read_std_mctq())
#' }
#' }
read_std_mctq <- function(
        id = "1dgbg96BpA9jS0O7dt8onGzHodgefNut7RAc4IwBvqJ8",
        sheet = "Raw data"
        ) {
    checkmate::assert_string(id)
    checkmate::assert_string(sheet)

    checkmate::assert_choice(sheet, googlesheets4::sheet_names(id))

    out <- googlesheets4::read_sheet(
        ss = id, sheet = sheet, col_names = TRUE, col_types = "c",
        na = c("", "NA"), trim_ws = TRUE
        ) %>%
        dplyr::rename_with(
            ~ c(
                "timestamp", "email",

                "work", "wd",

                "bt_w", "sprep_w", "slat_w", "se_w", "si_w", "alarm_w",
                "wake_before_w", "le_w",

                "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f",
                "reasons_why_f", "le_f"
            )
        )

    invisible(out)
}

#' Build a fictional standard MCTQ raw dataset
#'
#' @description
#'
#' `build_std_mctq()` builds a fictional raw dataset, __for testing and learning
#' purposes__, composed of basic/measurable variables of the Munich ChronoType
#' Questionnaire (MCTQ) standard version.
#'
#' @param n (optional) a integer number indicating how many random MCTQ cases to
#'   build.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a raw standard MCTQ
#'   dataset.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(build_random_std_mctq())
#' }
#' }
build_random_std_mctq <- function(n = 50) {
    checkmate::assert_int(n, lower = 1)

    cli::cli_progress_step("Building MCTQ data")

    out <- mctq::random_mctq(model = "standard") %>% dplyr::as_tibble()

    if (!n == 1) {
        cli::cli_progress_bar(total = n - 1, clear = TRUE)

        for (i in seq_len(n - 1)) {
            out <- out %>%
                dplyr::bind_rows(
                    mctq::random_mctq(model = "standard") %>%
                        dplyr::as_tibble()
                )

            cli::cli_progress_update()
        }
    }

    invisible(out)
}

#' Tidy `read_std_mctq()` output
#'
#' @description
#'
#' `tidy_std_mctq` tidy the output of `read_std_mctq()` or
#' `build_random_std_mctq()`.
#'
#' @details
#'
#' Here, the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo and Jonge (2018). It's a very similar
#' process of tiding data described in the workflow proposed by Wickham and
#' Grolemund (n.d.).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' data, run `validate_std_mctq()`.
#'
#' To learn more about the concept of tidy data, see Wickham (2014) and
#' Wickham and Grolemund (n.d.).
#'
#' @param data (optional) a [`tibble`][tibble::tibble()] with the
#'   `read_std_mctq()` or `build_random_std_mctq()` output.
#'
#' @return An invisible [`tibble`][tibble::tibble()] with a tidied, but not
#'   validated, standard MCTQ dataset.
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. \doi{10.1002/9781118897126}.
#'
#' Wickham, H. (2014). Tidy Data. _Journal of Statistical Software_, _59_(10),
#' 1-23. \doi{10.18637/jss.v059.i10}.
#'
#' Wickham, H., & Grolemund, G. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. \url{https://r4ds.had.co.nz}
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(tidy_std_mctq())
#' }
#' }
tidy_std_mctq <- function(data = read_std_mctq()) {
    checkmate::assert_tibble(data)

    if ("timestamp" %in% names(data)) {
        data_names <- c(
            "timestamp", "email",

            "work", "wd",

            "bt_w", "sprep_w", "slat_w", "se_w", "si_w", "alarm_w",
            "wake_before_w", "le_w",

            "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f",
            "reasons_why_f", "le_f"
        )

        checkmate::assert_set_equal(names(data), data_names)

        cli::cli_progress_step("Tyding MCTQ data")

        out <- data %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                length = dplyr::n_distinct(dplyr::c_across(
                    !dplyr::matches("^timestamp$|^email$")
                ))) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!length <= 2) %>%
            dplyr::select(-length) %>%
            dplyr::mutate(
                timestamp = lubridate::parse_date_time(
                    x = timestamp, orders = "m/d/Y H:M:S",
                    tz = "America/Sao_Paulo"
                    ),
                wd = mctq:::shush(as.integer(wd)),
                ) %>%
            dplyr::mutate(
                dplyr::across(
                    dplyr::matches("^bt_|^sprep_|^se_"),
                    ~ hms::as_hms(lubridate::parse_date_time(
                        x = .x, orders = "I:M:S p"
                    ))
                ),
                dplyr::across(
                    dplyr::matches("^slat_|^si_"),
                    ~ lubridate::dminutes(mctq:::shush(as.integer(.x)))
                ),
                dplyr::across(
                    dplyr::matches("^le_"),
                    ~ lubridate::dseconds(as.numeric(hms::as_hms(
                        mctq:::shush(lubridate::parse_date_time(
                            x = .x, "H:M:S"
                            )))))
                ),
                dplyr::across(
                    dplyr::matches("^work$|^alarm_|^wake_before_w$"),
                    ~ dplyr::case_when(
                        tolower(.x) == "sim" ~ TRUE,
                        tolower(.x) == "n\u00e3o" ~ FALSE,
                        TRUE ~ as.logical(NA))
                    )
            )
    } else {
        data_names <- c(
            "work", "wd",

            "bt_w", "sprep_w", "slat_w", "se_w", "si_w", "alarm_w",
            "wake_before_w", "le_w",

            "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f",
            "reasons_f", "reasons_why_f", "le_f"
        )

        checkmate::assert_set_equal(names(data), data_names)

        cli::cli_progress_step("Tyding MCTQ data")

        out <- data %>%
            dplyr::mutate(
                timestamp = rep(as.POSIXct(NA), nrow(data)),
                email = rep(as.character(NA), nrow(data))
            ) %>%
            dplyr::select(-reasons_f)
    }

    out <- out %>%
        dplyr::relocate(
            timestamp, email,

            work, wd,

            bt_w, sprep_w, slat_w, se_w, si_w, alarm_w, wake_before_w, le_w,

            bt_f, sprep_f, slat_f, se_f, si_f, alarm_f, reasons_why_f, le_f,
        ) %>%
        dplyr::arrange(timestamp)

    invisible(out)
}

#' Validate `tidy_std_mctq()` output
#'
#' @description
#'
#' `validate_std_mctq()` validates the output of `tidy_std_mctq()`.
#'
#' @details
#'
#' Here, the process of _validating_ a dataset is understood as detecting
#' invalid data, by checking whether data satisfies certain assumptions from
#' domain knowledge, to them, removing or, if possible, fixing them. You can
#' find more about data validation and error location in Loo and Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund (n.d.).
#'
#' @param data (optional) a [`tibble`][tibble::tibble()] with the
#'   `tidy_std_mctq()` output.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated standard
#'   MCTQ dataset.
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. \doi{10.1002/9781118897126}.
#'
#' Wickham, H., & Grolemund, G. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. \url{https://r4ds.had.co.nz}
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(validate_std_mctq())
#' }
#' }
validate_std_mctq <- function(data = tidy_std_mctq()) {
    data_names <- c(
        "timestamp", "email",

        "work", "wd",

        "bt_w", "sprep_w", "slat_w", "se_w", "si_w", "alarm_w", "wake_before_w",
        "le_w",

        "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f", "reasons_why_f",
        "le_f"
    )

    checkmate::assert_tibble(data)
    checkmate::assert_set_equal(names(data), data_names)

    cli::cli_progress_step("Validating MCTQ data")

    ## Do univariate validation

    out <- data %>%
        dplyr::mutate(
            wd = dplyr::case_when(
                validate::in_range(wd, min = 0, max = 7) ~ wd
                )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::matches("^bt_|^sprep_|^se_"),
                ~ dplyr::case_when(
                    .x == hms::parse_hm("24:00") ~ hms::parse_hm("00:00"),
                    .x >= hms::parse_hm("00:00") &
                        .x < hms::parse_hm("24:00") ~ .x
                    )
                ),
            dplyr::across(
                dplyr::matches("^slat_|^si_"),
                ~ dplyr::case_when(
                    validate::in_range(
                        .x,
                        min = lubridate::dhours(0),
                        max = lubridate::dhours(6)
                        ) ~ .x
                )
                ),
            dplyr::across(
                dplyr::matches("^le_"),
                ~ dplyr::case_when(
                    validate::in_range(
                        .x,
                        min = lubridate::dhours(0),
                        max = lubridate::dhours(24)
                        ) ~ .x
                    )
                )
        )

    ## Do multivariate validation

    for (i in c("_w", "_f")) {
        bt_i <- paste0("bt", i)
        sprep_i <- paste0("sprep", i)

        out <- out %>%
            dplyr::mutate(
                dummy = dplyr::case_when(
                    mctq::assign_date(!!as.symbol(bt_i), !!as.symbol(sprep_i)) >
                        lubridate::dhours(12) ~ TRUE,
                    TRUE ~ FALSE),
                bkp = !!as.symbol(bt_i),
                !!as.symbol(bt_i) :=
                    dplyr::if_else(dummy, !!as.symbol(sprep_i),
                                   !!as.symbol(bt_i)),
                !!as.symbol(sprep_i) :=
                    dplyr::if_else(dummy, bkp, !!as.symbol(sprep_i))) %>%
            dplyr::select(-dummy, -bkp)
    }

    for (i in c("_w", "_f")) {
        sprep_i <- paste0("sprep", i)
        slat_i <- paste0("slat", i)
        se_i <- paste0("se", i)

        test <- out %>%
            dplyr::mutate(
                so_i = mctq::so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
                sd_i = mctq::sdu(so_i, !!as.symbol(se_i)),
                dummy = dplyr::case_when(
                    sd_i < lubridate::dhours(2) |
                        sd_i > lubridate::dhours(18) ~ TRUE,
                    TRUE ~ FALSE)) %>%
            dplyr::select(dummy)

        out <- out %>%
            dplyr::bind_cols(test) %>%
            dplyr::mutate(dplyr::across(
                dplyr::ends_with(i),
                ~ dplyr::if_else(dummy, mctq:::na_as(.x), .x))
            ) %>%
            dplyr::select(-dummy)
    }

    ## Fix/impute linked data

    out <- out %>%
        dplyr::mutate(
            wd = dplyr::case_when(
                work == FALSE & is.na(wd) ~ as.integer(0),
                TRUE ~ wd),
            work = dplyr::case_when(
                work == FALSE & wd > 0 ~ TRUE,
                TRUE ~ work),
            wake_before_w = dplyr::case_when(
                alarm_w == FALSE ~ as.logical(NA),
                TRUE ~ wake_before_w),
            reasons_why_f = dplyr::case_when(
                tolower(reasons_why_f) == "no" ~ as.character(NA),
                TRUE ~ reasons_why_f)
        )

    invisible(out)
}

#' Analyze `validate_std_mctq()` output
#'
#' @description
#'
#' `analyse_std_mctq()` computes and creates the non-measured MCTQ variables
#' based on the output of `validate_std_mctq()`.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo and Jonge (2018). It's also a part of the
#' process of transforming data, described in the workflow proposed by Wickham
#' and Grolemund (n.d.).
#'
#' @param data (optional) a [`tibble`][tibble::tibble()] with the
#'   `validate_std_mctq()` output.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with all the variables
#'   proposed for a standard MCTQ dataset.
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. \doi{10.1002/9781118897126}.
#'
#' Wickham, H., & Grolemund, G. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. \url{https://r4ds.had.co.nz}
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(analyze_std_mctq())
#' }
#' }
analyze_std_mctq <- function(data = validate_std_mctq()) {
    data_names <- c(
        "timestamp", "email",

        "work", "wd",

        "bt_w", "sprep_w", "slat_w", "se_w", "si_w", "alarm_w", "wake_before_w",
        "le_w",

        "bt_f", "sprep_f", "slat_f", "se_f", "si_f", "alarm_f", "reasons_why_f",
        "le_f"
    )

    checkmate::assert_tibble(data)
    checkmate::assert_set_equal(names(data), data_names)

    cli::cli_progress_step("Analyzing MCTQ data")

    # Compute variables -----

    out <- data %>%
        dplyr::mutate(
            id = as.integer(seq_len(nrow(data))),
            reasons_f = dplyr::case_when(
                is.na(reasons_why_f) | reasons_why_f == "" ~ FALSE,
                grepl(
                    "^n\u00e3o$|^nao$", reasons_why_f, ignore.case = TRUE
                ) ~ FALSE,
                TRUE ~ TRUE)
        ) %>%
        dplyr::mutate(
            fd = mctq::fd(wd),
            so_w = mctq::so(sprep_w, slat_w),
            gu_w = mctq::gu(se_w, si_w),
            sd_w = mctq::sdu(so_w, se_w),
            tbt_w = mctq::tbt(bt_w, gu_w),
            msw = mctq::msl(so_w, sd_w),

            so_f = mctq::so(sprep_f, slat_f),
            gu_f = mctq::gu(se_f, si_f),
            sd_f = mctq::sdu(so_f, se_f),
            tbt_f = mctq::tbt(bt_f, gu_f),
            msf = mctq::msl(so_f, sd_f),

            sd_week = mctq::sd_week(sd_w, sd_f, wd),
            msf_sc = mctq::msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),
            sloss_week = mctq::sloss_week(sd_w, sd_f, wd),
            le_week = mctq::le_week(le_w, le_f, wd),
            sjl_rel = mctq::sjl_rel(msw, msf),
            sjl = abs(sjl_rel),
            sjl_sc_rel = mctq::sjl_sc_rel(so_w, se_w, so_f, se_f),
            sjl_sc = abs(sjl_sc_rel),
            chronotype = dplyr::case_when(
                msf_sc < hms::parse_hms("02:46:25") ~ "Extremely early",
                msf_sc >= hms::parse_hms("02:46:25") &
                    msf_sc < hms::parse_hms("03:20:00") ~ "Moderately early",
                msf_sc >= hms::parse_hms("03:20:00") &
                    msf_sc < hms::parse_hms("03:45:42") ~ "Slightly early",
                msf_sc >= hms::parse_hms("03:45:42") &
                    msf_sc < hms::parse_hms("05:05:00") ~ "Intermediate",
                msf_sc >= hms::parse_hms("05:05:00") &
                    msf_sc < hms::parse_hms("05:40:00") ~ "Slightly late",
                msf_sc >= hms::parse_hms("05:40:00") &
                    msf_sc < hms::parse_hms("06:30:00") ~ "Moderately late",
                msf_sc >= hms::parse_hms("06:30:00") ~ "Extremely late"
            ),
            chronotype = factor(
                x = chronotype, ordered = TRUE,
                levels = c(
                    "Extremely early", "Moderately early", "Slightly early",
                    "Intermediate", "Slightly late", "Moderately late",
                    "Extremely late"
                    )
                )
        ) %>%
        dplyr::relocate(
            id, timestamp, email,

            work, wd, fd,

            bt_w, sprep_w, slat_w, so_w, se_w, si_w, gu_w, alarm_w,
            wake_before_w, sd_w, tbt_w, le_w, msw,

            bt_f, sprep_f, slat_f, so_f, se_f, si_f, gu_f, alarm_f,
            reasons_f, reasons_why_f, sd_f, tbt_f, le_f, msf,

            sd_week, sloss_week, le_week, msf_sc, chronotype, sjl_rel, sjl,
            sjl_sc_rel, sjl_sc
        )

    # Fix missing sections -----

    ## See `vignette("missing-sections", pakckage = "mctq")` to learn more.

    count_w <- length(names(out)[grepl("_w$", names(out))])
    count_f <- length(names(out)[grepl("_f$", names(out))])
    count_w <- count_w * 2/3
    count_f <- count_f * 2/3

    count_na <- function(x) {
        checkmate::assert_atomic(x)

        length(which(is.na(x)))
    }

    test <- out %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            dummy_0_a = as.integer(wd) == 0,
            dummy_0_b = count_na(
                dplyr::c_across(dplyr::ends_with("_w"))
                ) >= count_w,
            dummy_0_c = alarm_f == FALSE,
            dummy_7_a = as.integer(wd) == 7,
            dummy_7_b = count_na(
                dplyr::c_across(dplyr::ends_with("_f"))) >= count_f,
            dummy_0 = dummy_0_a & dummy_0_b & dummy_0_c & dummy_7_b == FALSE,
            dummy_7 = dummy_7_a & dummy_7_b & dummy_0_b == FALSE
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(dummy_0, dummy_7)

    out <- dplyr::bind_cols(out, test) %>%
        dplyr::mutate(
            sd_week = dplyr::case_when(
                dummy_0 == TRUE ~ sd_f,
                dummy_7 == TRUE ~ sd_w,
                TRUE ~ sd_week
            ),
            msf_sc = dplyr::if_else(dummy_0, msf, msf_sc),
            sloss_week = dplyr::if_else(
                dummy_0, lubridate::dhours(0), sloss_week
            ),
            le_week = dplyr::case_when(
                dummy_0 == TRUE ~ le_f,
                dummy_7 == TRUE ~ le_w,
                TRUE ~ le_week
            ),
            sjl_rel = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_rel),
            sjl = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl),
            sjl_sc_rel = dplyr::if_else(
                dummy_0, lubridate::dhours(0), sjl_sc_rel
            ),
            sjl_sc = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_sc)
        ) %>%
        dplyr::select(-dummy_0, -dummy_7)

    invisible(out)
}

#' Write the standard MCTQ dataset to a Google Sheets
#'
#' @description
#'
#' `write_std_mctq()` writes a tidyied, validated and analysed dataset of the
#' Munich ChronoType Questionnaire (MCTQ) standard version to a Google
#' Spreadsheet.
#'
#' @param id (optional) a string indicating the Google Spreadsheet ID (default:
#'   `"1dgbg96BpA9jS0O7dt8onGzHodgefNut7RAc4IwBvqJ8"`).
#' @param sheet (optional) a string indicating the sheet name where the data
#'   must be written (default: `"Dataset"`).
#'
#' @return An invisible `NULL`. This function don't aim to return values.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' write_std_mctq()
#' }
write_std_mctq <- function(data = analyze_std_mctq(),
                           id = "1dgbg96BpA9jS0O7dt8onGzHodgefNut7RAc4IwBvqJ8",
                           sheet = "Dataset") {
    data_names <- c(
        "id", "timestamp", "email",

        "work", "wd", "fd",

        "bt_w", "sprep_w", "slat_w", "so_w", "se_w", "si_w", "gu_w", "alarm_w",
        "wake_before_w", "sd_w", "tbt_w", "le_w", "msw",

        "bt_f", "sprep_f", "slat_f", "so_f", "se_f", "si_f", "gu_f", "alarm_f",
        "reasons_f", "reasons_why_f", "sd_f", "tbt_f", "le_f", "msf",

        "sd_week", "sloss_week", "le_week", "msf_sc", "chronotype", "sjl_rel",
        "sjl", "sjl_sc_rel", "sjl_sc"
    )

    checkmate::assert_tibble(data)
    checkmate::assert_set_equal(names(data), data_names)

    out <- data %>%
        dplyr::mutate(
            dplyr::across(
                where(~ mctq:::test_hms(.x) || mctq:::test_duration(.x)),
                ~ as.numeric(mctq::round_time(.x)) / 60 / 60 / 24
                )
        )

    googlesheets4::sheet_resize(
        ss = id, sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
        )

    googlesheets4::range_write(
        ss = id, data = out, sheet = sheet, range = "A1", col_names = TRUE,
        reformat = FALSE
        )

    invisible(NULL)
}

# raw <- read_std_mctq()
# tidy <- tidy_std_mctq()
# valid <- validate_std_mctq()
# analysis <- analyze_std_mctq()

# raw <- build_std_mctq()
# tidy <- tidy_std_mctq(raw)
# valid <- validate_std_mctq(tidy)
# analysis <- analyze_std_mctq(valid)

# read_std_mctq() %>%
#     tidy_std_mctq() %>%
#     validate_std_mctq() %>%
#     analyze_std_mctq %>%
#     write_std_mctq()

# raw <- build_random_std_mctq(n = 100)
# raw %>%
#     tidy_std_mctq() %>%
#     validate_std_mctq() %>%
#     analyze_std_mctq  %>%
#     write_std_mctq()
