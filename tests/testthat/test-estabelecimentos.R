test_that("estabelecimentos_bairro avisa quando o tipo n\u00e3o existe", {
  falso <- data.table::data.table(
    CO_CNES = c("1", "2"), CO_IBGE = "330455", NO_FANTASIA = c("A", "B"),
    TP_UNIDADE = c("5", "2"), CO_CEP = "20000000", NO_BAIRRO = "CENTRO",
    NU_LATITUDE = "-22.9", NU_LONGITUDE = "-43.2", CO_MOTIVO_DESAB = "",
    CO_AMBULATORIAL_SUS = "SIM", ST_ATEND_HOSPITALAR = "")
  attr(falso, "data_cadastro") <- as.Date("2026-10-06")
  testthat::local_mocked_bindings(
    cnes_aberto = function() data.table::copy(falso),
    tipos_unidade_cnes = function() c("5" = "HOSPITAL GERAL", "2" = "CENTRO DE SAUDE/UNIDADE BASICA"),
    .package = "localdatasus")
  expect_error(estabelecimentos_bairro("Rio de Janeiro", tipo = "farmacia"), "LDS-10")
  expect_error(estabelecimentos_bairro("Rio de Janeiro", por = "sexo"), "LDS-10")
})
