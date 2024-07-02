# library(checkmate, quietly = TRUE)
# See: <https://pt.wikipedia.org/wiki/Cadastro_de_Pessoas_F%C3%ADsicas>

test_cpf <- function(cpf) {
  checkmate::assert_atomic(cpf)
  checkmate::assert_character(as.character(cpf), pattern = "^\\d{11}$")

  vapply(
    X = cpf,
    FUN = test_cpf_builder,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}

test_cpf_builder <- function(cpf) {
  checkmate::assert_atomic(cpf, len = 1)
  checkmate::assert_character(as.character(cpf), pattern = "^\\d{11}$")

  cpf <- as.integer(strsplit(cpf, "")[[1]])
  dv1 <- sum(cpf[1:9] * (10:2))
  dv1 <- ifelse(dv1 %% 11 < 2, 0, 11 - dv1 %% 11)
  dv2 <- sum(cpf[1:10] * (11:2))
  dv2 <- ifelse(dv2 %% 11 < 2, 0, 11 - dv2 %% 11)

  all(cpf[10:11] == c(dv1, dv2))
}
