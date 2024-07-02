# library(checkmate, quietly = TRUE)
# library(sodium, quietly = TRUE)
# library(stringr, quietly = TRUE)

anonymize_id <- function(x, salt) {
  checkmate::assert_atomic(x)
  checkmate::assert_character(as.character(x), pattern = "^\\d{11}$")
  checkmate::assert_string(salt)

  vapply(
    X = x,
    FUN = function(x, salt) {
      as.character(x) |>
        charToRaw() |>
        sodium::scrypt(salt = charToRaw(salt), size = 32) |>
        sodium::bin2hex() |>
        stringr::str_trunc(8, ellipsis = "")
    },
    FUN.VALUE = character(1),
    salt = salt,
    USE.NAMES = FALSE
  )
}
