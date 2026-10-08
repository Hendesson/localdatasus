test_that("a suavizacao puxa mais forte as taxas dos bairros pequenos", {
  x <- data.frame(ano = 2024, codigo_municipio = "261160",
                  bairro = c("A", "B", "C", "D"),
                  casos = c(500, 260, 130, 24), populacao = c(100000, 50000, 20000, 447))
  s <- suavizar_taxas(x)
  bruta <- 10000 * x$casos / x$populacao
  media <- 10000 * sum(x$casos) / sum(x$populacao)
  # O bairro pequeno (D) muda muito; o grande (A) quase nada.
  expect_lt(abs(s$taxa_suavizada_por_10mil[4] - media), abs(bruta[4] - media))
  expect_lt(abs(s$taxa_suavizada_por_10mil[1] - bruta[1]), abs(s$taxa_suavizada_por_10mil[4] - bruta[4]))
  # A ordem entre os bairros grandes se mantém.
  expect_gt(s$taxa_suavizada_por_10mil[2], s$taxa_suavizada_por_10mil[1])
})

test_that("a suavizacao e feita por municipio e ignora bairros sem populacao", {
  x <- data.frame(ano = 2024, codigo_municipio = c("1", "1", "2", "2", "2"),
                  bairro = c("A", "B", "C", "D", "E"),
                  obitos = c(10, 20, 100, 50, 3), populacao = c(10000, 10000, 10000, 10000, NA))
  s <- suavizar_taxas(x)
  expect_true(is.na(s$taxa_suavizada_por_10mil[5]))
  # Municipio 1 nao influencia o municipio 2 (medias bem diferentes).
  expect_lt(max(s$taxa_suavizada_por_10mil[1:2]), min(s$taxa_suavizada_por_10mil[3:4]))
})

test_that("sem coluna de contagem ou populacao, erro claro", {
  expect_error(suavizar_taxas(data.frame(bairro = "A", populacao = 10)), "LDS-17")
})
