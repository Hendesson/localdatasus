test_that("samu_bairro filtra o munic\u00edpio, descarta duplicadas e valida o por", {
  falso <- data.table::data.table(
    data = "2024-01-01", municipio = c("RECIFE", "RECIFE", "OLINDA", "RECIFE"),
    bairro = c("BOA VIAGEM", "BOA VIAGEM", "CARMO", "IBURA"),
    tipo = c("CAUSAS EXTERNAS", "GERAIS/OUTROS", "CAUSAS EXTERNAS", "CAUSAS EXTERNAS"),
    subtipo = "X", sexo = "F", origem_chamado = "VIA P\u00daBLICA",
    motivo_finalizacao = c("", "SOLICITA\u00c7\u00c3O DUPLICADA", "", ""), motivo_desfecho = "",
    ano = 2024L)
  capturado <- NULL
  testthat::local_mocked_bindings(
    baixar_samu = function(anos) data.table::copy(falso),
    ligar_bairros = function(nome, codmun, uf, ...) {
      capturado <<- nome
      data.frame(nome = nome, codmun = codmun, ligacao = "n\u00e3o encontrado", id_unidade = NA_character_,
                 nome_ibge = NA_character_, fonte_unidade = NA_character_, cd_ibge = NA_character_,
                 lat = NA_real_, lon = NA_real_,
                 populacao = NA_real_)
    },
    .package = "localdatasus")
  x <- suppressMessages(samu_bairro("Recife", 2024, tipo = "causas externas"))
  expect_setequal(capturado, c("BOA VIAGEM", "IBURA"))
  expect_equal(sum(x$chamados), 2)
  expect_error(samu_bairro("Recife", 2024, por = "cor"), "LDS-10")
})
