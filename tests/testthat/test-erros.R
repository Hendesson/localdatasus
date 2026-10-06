# Erros com código e ajuda().

test_that("erros levam o código e a dica de ajuda", {
  e <- tryCatch(localdatasus:::codigo_uf("XX"), error = function(e) e)
  expect_s3_class(e, "localdatasus_erro")
  expect_equal(e$codigo, "LDS-01")
  expect_match(conditionMessage(e), "[LDS-01]", fixed = TRUE)
  expect_match(conditionMessage(e), 'ajuda("LDS-01")', fixed = TRUE)
})

test_that("todo código usado no pacote está documentado em ajuda()", {
  ns <- asNamespace("localdatasus")
  funcoes <- Filter(function(f) is.function(get(f, ns)), ls(ns, all.names = TRUE))
  codigo <- unlist(lapply(funcoes, function(f) deparse(get(f, ns))))
  usados <- unique(unlist(regmatches(codigo, gregexpr("LDS-[0-9]{2}", codigo))))
  usados <- setdiff(usados, "LDS-99")
  expect_true(length(usados) > 10)
  expect_true(all(usados %in% localdatasus:::.erros$codigo))
})

test_that("ajuda() aceita o código de vários jeitos", {
  expect_output(print(localdatasus::ajuda("LDS-03")), "nome repetido")
  expect_output(print(localdatasus::ajuda("03")), "nome repetido")
  expect_output(print(localdatasus::ajuda(3)), "Como resolver")
  expect_output(print(localdatasus::ajuda()), "LDS-23")
  expect_message(localdatasus::ajuda("LDS-99"), "desconhecido")
})

test_that("cid inválida dá erro LDS-08", {
  form <- list(opcoes = list(), ids = character())
  cfg <- list(fonte = "T", filtro_capitulo = "x", filtro_cid3 = "y")
  e <- tryCatch(localdatasus:::filtros_cid(cfg, form, "I20-J10"), error = function(e) e)
  expect_equal(e$codigo, "LDS-08")
})

test_that("mapa_bairro pede pacotes ou dados com lugar", {
  e <- tryCatch(localdatasus:::preparar_mapa(data.frame(a = 1), NULL), error = function(e) e)
  expect_equal(e$codigo, "LDS-21")
})
