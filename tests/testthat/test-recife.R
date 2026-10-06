test_that("ler_csv_recife conserta linhas entre aspas e padroniza nomes antigos", {
  arq <- tempfile(fileext = ".csv")
  writeLines(c(
    "nu_notificacao,dt_notificacao,co_municipio_residencia,no_bairro_residencia,nome_logradouro_residencia,nu_cep_residencia,tp_classificacao_final",
    "1,2015-01-08,261160,AREIAS,\"AV, IZABEL\",50000000,10.0",
    "\"2,2015-01-09,261160,BOA VIAGEM,\"\"RUA X, 1\"\",,5.0\""
  ), arq)
  d <- localdatasus:::ler_csv_recife(arq)
  expect_equal(nrow(d), 2)
  expect_true(all(c("NU_NOTIFIC", "DT_NOTIFIC", "ID_MN_RESI", "NM_BAIRRO", "NU_CEP", "CLASSI_FIN") %in% names(d)))
  expect_equal(d$NM_BAIRRO, c("AREIAS", "BOA VIAGEM"))
  expect_equal(d$NM_LOGRADO[2], "RUA X, 1")
})

test_that("ler_csv_recife aceita o formato novo, com ponto e vírgula", {
  arq <- tempfile(fileext = ".csv")
  writeLines(c("NU_NOTIFIC;DT_NOTIFIC;ID_MN_RESI;NM_BAIRRO;NU_CEP;CLASSI_FIN",
               "9;11/03/2024;261160;IBURA;;5"), arq)
  d <- localdatasus:::ler_csv_recife(arq)
  expect_equal(d$NM_BAIRRO, "IBURA")
  expect_equal(d$CLASSI_FIN, "5")
})

test_that("os sistemas do Recife estão cadastrados", {
  s <- sistemas_bairro()
  expect_true(all(c("DENGUE-RECIFE", "CHIKUNGUNYA-RECIFE", "ZIKA-RECIFE") %in% s$sistema))
  expect_equal(unname(localdatasus:::.classi_dengue["5"]), "Descartado")
})
