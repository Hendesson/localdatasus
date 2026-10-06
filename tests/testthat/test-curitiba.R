test_that("somar_mes atravessa o ano", {
  expect_equal(localdatasus:::somar_mes("2025-11", 3), "2026-02")
  expect_equal(localdatasus:::somar_mes("2026-01", -1), "2025-12")
})

test_that("processar_arquivo_curitiba guarda os meses completos no cache", {
  fake <- tempfile(fileext = ".csv")
  writeLines(c(
    paste("Data do Atendimento;Data de Nascimento;Sexo;Tipo de Unidade;Descri\u00e7\u00e3o da Unidade",
          "C\u00f3digo do CID;Descri\u00e7\u00e3o do CBO;Desencadeou Internamento;Munic\u00edcio;Bairro", sep = ";"),
    "02/06/2026 10:00:00;15/06/2016 00:00:00;F;UPA;UPA X;J039;MEDICO PEDIATRA;Nao;CURITIBA;SAO BRAZ   ",
    "10/08/2026 10:00:00;01/01/1950 00:00:00;M;BASICO;US Y;I10;MEDICO CLINICO;Nao;CURITIBA;CAJURU",
    "01/09/2026 10:00:00;01/01/1990 00:00:00;M;BASICO;US Y;I10;MEDICO CLINICO;Nao;COLOMBO;CENTRO"
  ), fake, useBytes = TRUE)
  x <- readLines(fake, encoding = "UTF-8")
  writeLines(iconv(x, "UTF-8", "latin1"), fake, useBytes = TRUE)
  pasta <- tempfile(); dir.create(pasta)
  testthat::local_mocked_bindings(baixar = function(url, destino) { file.copy(fake, destino); destino },
                                  .package = "localdatasus")
  feitos <- localdatasus:::processar_arquivo_curitiba("x.csv", "2026-09", pasta)
  expect_setequal(feitos, c("2026-06", "2026-08"))
  junho <- readRDS(file.path(pasta, "2026-06.rds"))
  expect_equal(junho$bairro, "SAO BRAZ")
  expect_equal(junho$idade, 9L)
  expect_false(file.exists(file.path(pasta, "2026-09.rds")))
})
